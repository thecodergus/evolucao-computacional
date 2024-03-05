// mod populacao;
// use populacao::Populacao;

use rand::distributions::{Distribution, Uniform};
use rand::Rng;
// Define uma enumeração para diferentes representações de dados.
#[derive(Debug)]
pub enum Representation {
    Binary { bits: u8 },              // Representação binária com número de bits
    Integer { bounds: (i32, i32) },   // Representação de números inteiros com limites
    PermutationInteger { size: u32 }, // Representação de permutação de números inteiros com tamanho
    Real { bounds: (f64, f64), dimensions: u32 }, // Representação de números reais com limites e dimensões
}

// Define uma estrutura para armazenar parâmetros do algoritmo
#[derive(Debug)]
pub struct Parameters {
    pub cod: Representation, // Representação dos dados
    pub run: u32,            // Número de iterações
    pub gen: u32,            // Número de gerações
    pub pop: u32,            // Tamanho da população
    pub dim: u32,            // Dimensão dos dados
}

impl Parameters {
    // Método para criar novos parâmetros
    pub fn new(cod: Representation, run: u32, gen: u32, pop: u32, dim: u32) -> Self {
        Parameters {
            cod,
            run,
            gen,
            pop,
            dim,
        }
    }

    // Método para gerar população inicial de acordo com a representação
    pub fn generate_population(&self) -> Vec<Vec<f64>> {
        match &self.cod {
            // Se a representação for binária
            Representation::Binary { bits } => {
                let mut rng = rand::thread_rng(); // Inicializa um gerador de números aleatórios
                let population: Vec<Vec<f64>> = (0..self.pop)
                    .map(|_| {
                        (0..self.dim)
                            .map(|_| rng.gen::<u32>() as f64 % (1 << bits) as f64) // Gera números aleatórios entre 0 e 2^bits - 1
                            .collect()
                    })
                    .collect();
                population
            }
            // Se a representação for de inteiros
            Representation::Integer { bounds } => {
                let mut rng = rand::thread_rng(); // Inicializa um gerador de números aleatórios
                let uniform_dist = Uniform::from(bounds.0..=bounds.1); // Define uma distribuição uniforme dentro dos limites especificados
                let population: Vec<Vec<f64>> = (0..self.pop)
                    .map(|_| {
                        (0..self.dim)
                            .map(|_| uniform_dist.sample(&mut rng) as f64) // Amostra números aleatórios de acordo com a distribuição uniforme
                            .collect()
                    })
                    .collect();
                population
            }
            // Se a representação for de permutação de inteiros
            Representation::PermutationInteger { size } => {
                let mut rng = rand::thread_rng(); // Inicializa um gerador de números aleatórios
                let mut population: Vec<Vec<f64>> = vec![];
                for _ in 0..self.pop {
                    let mut individual: Vec<f64> = (0..*size).map(|_| rng.gen::<f64>()).collect(); // Gera uma permutação aleatória
                    individual.sort_by(|a, b| a.partial_cmp(b).unwrap()); // Ordena a permutação
                    population.push(individual);
                }
                population
            }
            // Se a representação for de números reais
            Representation::Real { bounds, dimensions } => {
                let mut rng = rand::thread_rng(); // Inicializa um gerador de números aleatórios
                let uniform_dist = Uniform::from(bounds.0..=bounds.1); // Define uma distribuição uniforme dentro dos limites especificados
                let population: Vec<Vec<f64>> = (0..self.pop)
                    .map(|_| {
                        (0..*dimensions)
                            .map(|_| uniform_dist.sample(&mut rng)) // Amostra números aleatórios de acordo com a distribuição uniforme
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
