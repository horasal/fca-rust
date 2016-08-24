extern crate bitvector;
extern crate rustc_serialize;
extern crate csv;

pub mod formalconcept;

#[cfg(test)]
mod test {
    use super::formalconcept::*;
    use bitvector::*;
    #[test]
    fn test_creation() {
        let mut fc = ConceptLattice::<String, String>::new();
        fc.read_csv(include_str!("testdata.txt"), b',');
        assert_eq!(fc.attributes.len(), 7);
        assert_eq!(fc.objects.len(), 4);
        assert_eq!(fc.objects[1].children.iter().collect::<Vec<_>>(), 
                vec![0,1,2,3]);
        assert_eq!(fc.attributes[6].children.iter().collect::<Vec<_>>(), 
                vec![2,3]);
    }

    #[test]
    fn test_enumerate() {
        let mut fc = ConceptLattice::<String, String>::new();
        fc.read_csv(include_str!("testdata.txt"), b',');
        fc.enumerate_seq().enumerate();
        assert_eq!(fc.concepts.len(), 8);
    }

    #[test]
    fn test_up_and_down() {
        let mut fc = ConceptLattice::<String, String>::new();
        fc.read_csv(include_str!("testdata.txt"), b',');
        assert_eq!(fc.down(&vec![0, 0, 1, 0, 0, 0, 0]
              .into_iter().map(|x| x == 1).collect::<BitVector>()),
                vec![1, 1, 0, 0]
                    .into_iter().map(|x| x == 1).collect::<BitVector>());
        assert_eq!(fc.up(&vec![1, 1, 0, 0]
                         .into_iter().map(|x| x == 1).collect::<BitVector>()),
                    vec![1, 0, 1, 0, 0, 0, 0]
                        .into_iter().map(|x| x == 1).collect::<BitVector>());
    }

    #[test]
    fn test_iterator() {
        let mut fc = ConceptLattice::<String, String>::new();
        fc.read_csv(include_str!("testdata.txt"), b',');
        assert_eq!(fc.enumerate_seq().iter().collect::<Vec<_>>().len(), 8);
    }
}
