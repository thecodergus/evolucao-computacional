mod individuo;
use individuo::Individuo;
use std::ops::AddAssign;

#[derive(Clone, Debug)]
pub struct Populacao<T: Copy + AddAssign + PartialOrd + PartialEq> {
    tamanho: usize,
    individuos: Vec<T>,
}

impl<T: Copy + AddAssign + PartialOrd + PartialEq> Populacao<T> {
    pub fn new(tamanho: usize) -> Populacao<T> {
        Populacao {
            tamanho,
            individuos: Vec::with_capacity(tamanho),
        }
    }
}

trait CriarPopulacao<T> {
    fn bounds(&mut self, min: T, max: T) -> &Self;
    fn permutation(&mut self, values: Vec<T>) -> &Self;
}

impl<T: Copy + AddAssign + PartialOrd + PartialEq> CriarPopulacao<T> for Populacao<T> {
    fn bounds(&mut self, min: T, max: T) -> &Self {
        let mut rng = rand::thread_rng();

        for _ in 0..self.tamanho {
            let individuo: Individuo<T> = Individuo::new();
            individuo.bounds(min, max);
        }

        self
    }

    fn permutation(&mut self, mut values: Vec<T>) -> &Self {
        let mut rng = rand::thread_rng();

        values.shuffle(&mut rng);

        self.individuos = values;

        self
    }
}
