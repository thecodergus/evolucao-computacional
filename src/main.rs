// mod populacao;
// use populacao::Populacao;

use rand::distributions::{Distribution, Uniform};
use rand::Rng;

#[derive(Debug)]
pub enum Representation {
    Binary { bits: u8 },
    Integer { bounds: (i32, i32) },
    PermutationInteger { size: u32 },
    Real { bounds: (f64, f64), dimensions: u32 },
}

#[derive(Debug)]
pub struct Parameters {
    pub cod: Representation,
    pub run: u32,
    pub gen: u32,
    pub pop: u32,
    pub dim: u32,
}

impl Parameters {
    pub fn new(cod: Representation, run: u32, gen: u32, pop: u32, dim: u32) -> Self {
        Parameters {
            cod,
            run,
            gen,
            pop,
            dim,
        }
    }

    pub fn generate_population(&self) -> Vec<Vec<f64>> {
        match &self.cod {
            Representation::Binary { bits } => {
                let mut rng = rand::thread_rng();
                let population: Vec<Vec<f64>> = (0..self.pop)
                    .map(|_| {
                        (0..self.dim)
                            .map(|_| rng.gen::<u32>() as f64 % (1 << bits) as f64)
                            .collect()
                    })
                    .collect();
                population
            }
            Representation::Integer { bounds } => {
                let mut rng = rand::thread_rng();
                let uniform_dist = Uniform::from(bounds.0..=bounds.1);
                let population: Vec<Vec<f64>> = (0..self.pop)
                    .map(|_| {
                        (0..self.dim)
                            .map(|_| uniform_dist.sample(&mut rng) as f64)
                            .collect()
                    })
                    .collect();
                population
            }
            Representation::PermutationInteger { size } => {
                let mut rng = rand::thread_rng();
                let mut population: Vec<Vec<f64>> = vec![];
                for _ in 0..self.pop {
                    let mut individual: Vec<f64> = (0..*size).map(|_| rng.gen::<f64>()).collect();
                    individual.sort_by(|a, b| a.partial_cmp(b).unwrap());
                    population.push(individual);
                }
                population
            }
            Representation::Real { bounds, dimensions } => {
                let mut rng = rand::thread_rng();
                let uniform_dist = Uniform::from(bounds.0..=bounds.1);
                let population: Vec<Vec<f64>> = (0..self.pop)
                    .map(|_| {
                        (0..*dimensions)
                            .map(|_| uniform_dist.sample(&mut rng))
                            .collect()
                    })
                    .collect();
                population
            }
        }
    }
}

fn main() {
    let real_representation = Representation::Real {
        bounds: (-10.0, 10.0),
        dimensions: 2,
    };

    let params = Parameters::new(real_representation, 10, 50, 100, 2);

    let population = params.generate_population();
    println!("{:?}", population);
}
