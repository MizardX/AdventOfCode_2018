use std::collections::BinaryHeap;
use std::fmt::Display;
use std::num::{NonZero, NonZeroU32, ParseIntError};
use std::str::FromStr;

use thiserror::Error;

#[derive(Debug, Error)]
enum ParseError {
    #[error("Syntax error")]
    SyntaxError,
    #[error("Unknown damage type")]
    UnknownDamageType,
    #[error(transparent)]
    InvalidNumber(#[from] ParseIntError),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DamageType {
    Radiation,
    Bludgeoning,
    Fire,
    Cold,
    Slashing,
}

impl DamageType {
    const fn all() -> [Self; 5] {
        [
            Self::Radiation,
            Self::Bludgeoning,
            Self::Fire,
            Self::Cold,
            Self::Slashing,
        ]
    }
}

impl FromStr for DamageType {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "radiation" => Self::Radiation,
            "bludgeoning" => Self::Bludgeoning,
            "fire" => Self::Fire,
            "cold" => Self::Cold,
            "slashing" => Self::Slashing,
            _ => return Err(ParseError::UnknownDamageType),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Army {
    ImmuneSystem,
    Infection,
}

impl FromStr for Army {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "Immune System" => Self::ImmuneSystem,
            "Infection" => Self::Infection,
            _ => return Err(ParseError::SyntaxError),
        })
    }
}

impl Display for Army {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ImmuneSystem => f.write_str("Immune System"),
            Self::Infection => f.write_str("Infection"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Sensitivity {
    Normal,
    Weak,
    Immune,
}

impl Sensitivity {
    const fn multiplier(self) -> u32 {
        match self {
            Self::Immune => 0,
            Self::Normal => 1,
            Self::Weak => 2,
        }
    }
}

impl FromStr for Sensitivity {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "weak" => Self::Weak,
            "immune" => Self::Immune,
            _ => return Err(ParseError::SyntaxError),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Group {
    army: Army,
    id: u32,
    units: u32,
    hit_points: u32,
    sensitivities: [Sensitivity; DamageType::all().len()],
    attack_power: u32,
    attack_type: DamageType,
    initiative: u32,
}

impl FromStr for Group {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (units, rest) = s
            .split_once(" units each with ")
            .ok_or(ParseError::SyntaxError)?;
        let units = units.parse()?;
        let (hit_points, rest) = rest
            .split_once(" hit points ")
            .ok_or(ParseError::SyntaxError)?;
        let hit_points = hit_points.parse()?;
        let mut sensitivities = [Sensitivity::Normal; DamageType::all().len()];
        let rest = if let Some(rest) = rest.strip_prefix('(') {
            let (sensitivities_str, rest) = rest.split_once(") ").ok_or(ParseError::SyntaxError)?;
            for part in sensitivities_str.split("; ") {
                let (sensitivity, damage_types) =
                    part.split_once(" to ").ok_or(ParseError::SyntaxError)?;
                let sensensitivity: Sensitivity = sensitivity.parse()?;
                for damage_type in damage_types.split(", ") {
                    let damage_type: DamageType = damage_type.parse()?;
                    sensitivities[damage_type as usize] = sensensitivity;
                }
            }
            rest
        } else {
            rest
        };
        let (attack_power, rest) = rest
            .strip_prefix("with an attack that does ")
            .ok_or(ParseError::SyntaxError)?
            .split_once(' ')
            .ok_or(ParseError::SyntaxError)?;
        let attack_power = attack_power.parse()?;
        let (attack_type, initiative) = rest
            .split_once(" damage at initiative ")
            .ok_or(ParseError::SyntaxError)?;
        let attack_type = attack_type.parse()?;
        let initiative = initiative.parse()?;
        Ok(Self {
            army: Army::ImmuneSystem, // Unknown at at this point
            id: 0,                    // Unknown at this point
            units,
            hit_points,
            sensitivities,
            attack_power,
            attack_type,
            initiative,
        })
    }
}

#[derive(Debug, Clone)]
struct InitialConditions {
    groups: Vec<Group>,
}

impl FromStr for InitialConditions {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut groups = Vec::new();
        let mut army = Army::ImmuneSystem;
        let mut id = 1;
        for line in s.lines() {
            if line.is_empty() {
                continue;
            }
            if let Some(army_str) = line.strip_suffix(':') {
                army = army_str.parse()?;
                id = 1;
            } else if !line.is_empty() {
                let mut group: Group = line.parse()?;
                group.army = army;
                group.id = id;
                id += 1;
                groups.push(group);
            }
        }
        Ok(Self { groups })
    }
}

struct SimulationGroup<'a> {
    info: &'a Group,
    units_remaining: Option<NonZeroU32>,
    boost_attack_power: u32,
    current_target: Option<usize>,
    already_targeted: bool,
}

impl<'a> SimulationGroup<'a> {
    const fn new(info: &'a Group) -> Self {
        Self {
            info,
            units_remaining: NonZero::new(info.units),
            boost_attack_power: 0,
            current_target: None,
            already_targeted: false,
        }
    }

    fn effective_power(&self) -> u32 {
        (self.info.attack_power + self.boost_attack_power)
            * self.units_remaining.map_or(0, NonZero::get)
    }

    const fn damage_taken(&self, damage_type: DamageType, effective_power: u32) -> u32 {
        self.info.sensitivities[damage_type as usize].multiplier() * effective_power
    }

    const fn is_alive(&self) -> bool {
        self.units_remaining.is_some()
    }

    const fn is_dead(&self) -> bool {
        self.units_remaining.is_none()
    }

    const fn is_enemy_of(&self, other: &Self) -> bool {
        matches!(
            (self.info.army, other.info.army),
            (Army::ImmuneSystem, Army::Infection) | (Army::Infection, Army::ImmuneSystem)
        )
    }

    const fn take_damage(&mut self, effective_power: u32, attack_type: DamageType) -> u32 {
        if let Some(units) = self.units_remaining {
            let actual_damage =
                effective_power * self.info.sensitivities[attack_type as usize].multiplier();
            let units_killed = actual_damage / self.info.hit_points; // rounded down
            if units_killed >= units.get() {
                self.units_remaining = None;
                units.get()
            } else {
                self.units_remaining = NonZero::new(units.get() - units_killed);
                units_killed
            }
        } else {
            0
        }
    }
}

struct BattleSimulator<'a> {
    groups: Vec<SimulationGroup<'a>>,
    log: bool,
}

impl<'a> BattleSimulator<'a> {
    fn new(initial_conditions: &'a InitialConditions) -> Self {
        let groups = initial_conditions
            .groups
            .iter()
            .map(SimulationGroup::new)
            .collect();
        Self { groups, log: false }
    }

    fn reset(&mut self, boost: u32) {
        for group in &mut self.groups {
            group.already_targeted = false;
            group.current_target = None;
            group.units_remaining = NonZero::new(group.info.units);
            if group.info.army == Army::ImmuneSystem {
                group.boost_attack_power = boost;
            }
        }
    }

    fn select_targets(&mut self) {
        if self.log {
            println!();
        }
        for group in &mut self.groups {
            group.current_target = None;
            group.already_targeted = false;
        }
        if self.log {
            for group in &self.groups {
                if group.is_alive() {
                    let max_effective_damage = self
                        .groups
                        .iter()
                        .filter(|g| g.is_alive() && g.is_enemy_of(group))
                        .map(|g| g.damage_taken(group.info.attack_type, group.effective_power()))
                        .max()
                        .unwrap_or(0);
                    if max_effective_damage == 0 {
                        continue;
                    }
                    for enemy in &self.groups {
                        if enemy.is_alive() && enemy.is_enemy_of(group) {
                            let effective_damage =
                                enemy.damage_taken(group.info.attack_type, group.effective_power());
                            if effective_damage == max_effective_damage {
                                println!(
                                    "{} group {} would deal defending group {} {effective_damage} damage",
                                    group.info.army, group.info.id, enemy.info.id
                                );
                            }
                        }
                    }
                }
            }
        }
        let mut ordered: BinaryHeap<_> = self
            .groups
            .iter()
            .enumerate()
            .filter(|&(_, g)| g.is_alive())
            .map(|(ix, g)| (g.effective_power(), g.info.initiative, ix))
            .collect();
        while let Some((_, _, ix)) = ordered.pop() {
            let current = &self.groups[ix];
            let target = self
                .groups
                .iter()
                .enumerate()
                .filter(|&(_, enemy)| {
                    enemy.is_alive()
                        && enemy.is_enemy_of(current)
                        && !enemy.already_targeted
                        && enemy.damage_taken(current.info.attack_type, current.effective_power())
                            > 0
                })
                .max_by_key(|&(_, enemy)| {
                    (
                        enemy.damage_taken(current.info.attack_type, current.effective_power()),
                        enemy.effective_power(),
                        enemy.info.initiative,
                    )
                })
                .map(|(ix, _)| ix);
            if let Some(target_ix) = target {
                self.groups[ix].current_target = Some(target_ix);
                self.groups[target_ix].already_targeted = true;
            }
        }
    }

    fn attack_targets(&mut self) -> u32 {
        if self.log {
            println!();
        }
        let mut ordered: BinaryHeap<_> = self
            .groups
            .iter()
            .enumerate()
            .filter(|(_, g)| g.is_alive() && g.current_target.is_some())
            .map(|(ix, g)| (g.info.initiative, ix))
            .collect();
        let mut total_killed = 0;
        while let Some((_, ix)) = ordered.pop() {
            let current = &self.groups[ix];
            if current.is_dead() {
                continue;
            }
            let target_ix = current.current_target.unwrap();
            let target = &self.groups[target_ix];
            if target.is_dead() {
                continue;
            }
            if self.log {
                print!(
                    "{} group {} attacks defending group {}",
                    current.info.army, current.info.id, target.info.id
                );
            }
            let effective_power = current.effective_power();
            let attack_type = current.info.attack_type;
            let killed = self.groups[target_ix].take_damage(effective_power, attack_type);
            total_killed += killed;
            if self.log {
                println!(", killing {killed} units");
            }
        }
        total_killed
    }

    #[expect(unused)]
    fn is_game_over(&self) -> bool {
        self.get_winner().is_some()
    }

    fn get_winner(&self) -> Option<Army> {
        let mut army_counts = [0; 2];
        for group in &self.groups {
            if group.is_alive() {
                army_counts[group.info.army as usize] += 1;
            }
        }
        if army_counts[Army::ImmuneSystem as usize] == 0 {
            Some(Army::Infection)
        } else if army_counts[Army::Infection as usize] == 0 {
            Some(Army::ImmuneSystem)
        } else {
            None
        }
    }

    fn play(&mut self, boost: u32) -> u32 {
        self.reset(boost);
        loop {
            let mut battle_complete = false;
            for chunk in self.groups.chunk_by(|g1, g2| g1.info.army == g2.info.army) {
                if self.log {
                    println!("{}:", chunk[0].info.army);
                }
                let mut any_alive = false;
                for group in chunk {
                    if group.is_alive() {
                        if self.log {
                            println!(
                                "Group {} contains {} units",
                                group.info.id,
                                group.units_remaining.map_or(0, NonZero::get)
                            );
                        }
                        any_alive = true;
                    }
                }
                if !any_alive {
                    if self.log {
                        println!("No groups remain.");
                    }
                    battle_complete = true;
                }
            }
            if battle_complete {
                break;
            }
            self.select_targets();
            let units_killed = self.attack_targets();
            if units_killed == 0 {
                break;
            }
            if self.log {
                println!();
            }
        }
        self.groups
            .iter()
            .filter_map(|g| g.units_remaining.map(NonZero::get))
            .sum()
    }
}

#[aoc_generator(day24)]
fn parse(input: &str) -> Result<InitialConditions, ParseError> {
    input.parse()
}

#[aoc(day24, part1)]
fn part_1(initial_conditions: &InitialConditions) -> u32 {
    let mut sim = BattleSimulator::new(initial_conditions);
    sim.play(0)
}

#[aoc(day24, part2)]
fn part_2(initial_conditions: &InitialConditions) -> u32 {
    let mut sim = BattleSimulator::new(initial_conditions);
    let mut high = 100;
    let mut low = 0;
    sim.play(high);
    while sim.get_winner() == Some(Army::Infection) {
        low = high;
        high *= 2;
        sim.play(high);
    }
    while low < high {
        let mid = u32::midpoint(low, high);
        sim.play(mid);
        if sim.get_winner() == Some(Army::ImmuneSystem) {
            high = mid;
        } else {
            low = mid + 1;
        }
    }
    sim.play(low)
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE1: &str = "\
        18 units each with 729 hit points (weak to fire; immune to cold, slashing) \
        with an attack that does 8 radiation damage at initiative 10\
    ";

    const EXAMPLE2: &str = "\
        Immune System:\n\
        17 units each with 5390 hit points (weak to radiation, bludgeoning) with \
        an attack that does 4507 fire damage at initiative 2\n\
        989 units each with 1274 hit points (immune to fire; weak to bludgeoning, \
        slashing) with an attack that does 25 slashing damage at initiative 3\n\
        \n\
        Infection:\n\
        801 units each with 4706 hit points (weak to radiation) with an attack \
        that does 116 bludgeoning damage at initiative 1\n\
        4485 units each with 2961 hit points (immune to radiation; weak to fire, \
        cold) with an attack that does 12 slashing damage at initiative 4\
    ";

    #[test]
    fn test_parse() {
        let result = parse(EXAMPLE2).unwrap();
        assert_eq!(
            result.groups,
            [
                Group {
                    army: Army::ImmuneSystem,
                    id: 1,
                    units: 17,
                    hit_points: 5390,
                    sensitivities: SensitivityBuilder::new()
                        .add(DamageType::Radiation, Sensitivity::Weak)
                        .add(DamageType::Bludgeoning, Sensitivity::Weak)
                        .build(),
                    attack_power: 4507,
                    attack_type: DamageType::Fire,
                    initiative: 2,
                },
                Group {
                    army: Army::ImmuneSystem,
                    id: 2,
                    units: 989,
                    hit_points: 1274,
                    sensitivities: SensitivityBuilder::new()
                        .add(DamageType::Fire, Sensitivity::Immune)
                        .add(DamageType::Bludgeoning, Sensitivity::Weak)
                        .add(DamageType::Slashing, Sensitivity::Weak)
                        .build(),
                    attack_power: 25,
                    attack_type: DamageType::Slashing,
                    initiative: 3,
                },
                Group {
                    army: Army::Infection,
                    id: 1,
                    units: 801,
                    hit_points: 4706,
                    sensitivities: SensitivityBuilder::new()
                        .add(DamageType::Radiation, Sensitivity::Weak)
                        .build(),
                    attack_power: 116,
                    attack_type: DamageType::Bludgeoning,
                    initiative: 1,
                },
                Group {
                    army: Army::Infection,
                    id: 2,
                    units: 4485,
                    hit_points: 2961,
                    sensitivities: SensitivityBuilder::new()
                        .add(DamageType::Radiation, Sensitivity::Immune)
                        .add(DamageType::Fire, Sensitivity::Weak)
                        .add(DamageType::Cold, Sensitivity::Weak)
                        .build(),
                    attack_power: 12,
                    attack_type: DamageType::Slashing,
                    initiative: 4,
                }
            ]
        );
    }

    #[test]
    fn test_group() {
        let group: Group = EXAMPLE1.parse().unwrap();
        let simulation_grop = SimulationGroup {
            info: &group,
            units_remaining: NonZero::new(group.units),
            boost_attack_power: 0,
            current_target: None,
            already_targeted: false,
        };
        assert_eq!(simulation_grop.effective_power(), 144);
        assert_eq!(simulation_grop.damage_taken(DamageType::Fire, 144), 288);
        assert_eq!(simulation_grop.damage_taken(DamageType::Cold, 144), 0);
        assert_eq!(
            simulation_grop.damage_taken(DamageType::Bludgeoning, 144),
            144
        );
    }

    #[test]
    fn test_part_1() {
        let initial_conditions = parse(EXAMPLE2).unwrap();
        let result = part_1(&initial_conditions);
        assert_eq!(result, 5_216);
    }

    #[test]
    fn test_boost() {
        let initial_conditions = parse(EXAMPLE2).unwrap();
        let mut sim = BattleSimulator::new(&initial_conditions);
        sim.reset(1570);
        let effective_power = sim
            .groups
            .iter()
            .map(SimulationGroup::effective_power)
            .collect::<Vec<_>>();
        assert_eq!(
            effective_power,
            [17 * 6077, 989 * 1595, 801 * 116, 4485 * 12]
        );
    }

    #[test]
    fn test_part_2_fixed_boost() {
        let initial_conditions = parse(EXAMPLE2).unwrap();
        let mut sim = BattleSimulator::new(&initial_conditions);
        let result = sim.play(1570);
        assert_eq!(result, 51);
    }

    #[test]
    fn test_part_2() {
        let initial_conditions = parse(EXAMPLE2).unwrap();
        let result = part_2(&initial_conditions);
        assert_eq!(result, 51);
    }

    #[derive(Debug, Clone, Copy)]
    struct SensitivityBuilder {
        sensitivities: [Sensitivity; DamageType::all().len()],
    }

    impl SensitivityBuilder {
        fn new() -> Self {
            Self {
                sensitivities: [Sensitivity::Normal; _],
            }
        }

        fn add(mut self, damage_type: DamageType, sensitivity: Sensitivity) -> Self {
            self.sensitivities[damage_type as usize] = sensitivity;
            self
        }

        fn build(self) -> [Sensitivity; DamageType::all().len()] {
            self.sensitivities
        }
    }
}
