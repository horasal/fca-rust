//! ### Formal Concept Enumerator
//! 
//! A module for generating formal concepts lattice from 
//! object-attribute table. The data should be given
//! like the following csv form:
//!
//! > Header, attribute_name1, attribute_name2, attribute_name3, ..
//!
//! > object_name1, 0,           1,              0,            ..
//!
//! > object_name2, 1,          0,               0,            ..
//!
//! > ...
//!
//! These module provides two algorithms: sequential version and 
//! parallelized version, both are described in the paper
//!
//! `Parallel Recursive Algorithm for FCA, Petr Krajca, Jan Outrata and Vilem Vychodil`. 
//!
//!
//! ### Example
//!
//! ```
//! use fca::formalconcept::*;
//! let data = "header, a1, a2, a3
//! o1, 0, 0, 1
//! o2, 1, 1, 0
//! o3, 0, 1, 0
//! o4, 1, 0, 1
//! o5, 0, 0, 1";
//! // build a concept lattice with 
//! // attributes are strings
//! // objects are strings
//! let mut fc = ConceptLattice::<String, String>::new();
//! // read from csv string, data is separated by ','
//! fc.read_csv(data, b',');
//! // enumerate all possible concepts
//! fc.enumerate_seq().enumerate();
//! ```
//!
//! ### Performance
//!
//! #### Concepts Enumeration 
//!
//! On data with size = 958 * 29 / density = 34%
//!
//! * This Crate = 0.24s
//! * Ganter's in C = 2s
//! * Lindig's in C = 15s
//! * Berry's in C = 6s
//!
use bitvector::*;
use csv::{Reader, Result};
use std::fmt;

/// CSVRead Concept Trait
///
/// Any concept impls this trait means that the concept
/// can be read from a csv formatted relation table.
/// The table usually look like this:
///
/// table_name, header1, header2, header3, ...
/// element1,   0,       0,       1,       ...
/// element2,   1,       1,       0,       ...
/// element3,   0,       1,       0,       ...
/// ...  
pub trait CSVRead {
    fn read_csv(&mut self, content: &str, sep: u8);
}

/// FIMIRead Trait
///
/// Any concepts impls this trait means that the concept
/// can be read from a fimi formatted relation table.
/// Each line of the table represent a attribute-list
/// of one object. Attribute are always represented 
/// in a number counting start from 0.
/// no header is allowed.
///
/// FIMI Format is much more compact that csv format, 
/// because it only save the existing attributes.
///
/// The table usually looks like:
///
/// 0 1 2 ...
/// 1 2 5 ...
/// 0 1 5 ...
/// ...
pub trait FIMIRead {
    fn read_fimi(&mut self, content: &str, sep: u8);
}


/// FormalConcept Trait
///
/// A collection of basic operators of Formal Concept, including:
/// 
/// * A↑ : the set of all attributes shared by all objects from A
/// * B↓ : the set of all objects sharing all attributes from B
/// * Closure: get a new formal concept from existing concept by adding a attribute
pub trait FormalConcept {
    fn up(&self, obj: &BitVector) -> BitVector;
    fn down(&self, attr: &BitVector) -> BitVector;
    fn closure_extend(&self, concept: &Concept, new_attr: &usize) -> Concept;
}

pub trait Enumerator { fn enumerate(mut self); }

/// Concept lattice
/// 
/// ConceptLattice contains:
/// 
/// * a list of objects
/// * a list of attributes
/// * a list of all concepts
///
/// A common flow of generating conceptlattices is that:
///
/// * Read the objects and attributes from a csv file.
/// * Run `enumerate_concept` to generate a list of concepts
pub struct ConceptLattice <T, K> {
    /// list of the objects
    pub objects: Vec<Item<T>>,
    /// list of the attributes
    pub attributes: Vec<Item<K>>,
    /// list of all concepts, created by `enumerate_concepts`
    pub concepts: Vec<Concept>, 
}

impl<T,K> ConceptLattice<T,K> {
    pub fn new() -> Self {
        ConceptLattice {
            objects: Vec::new(),
            attributes: Vec::new(),
            concepts: Vec::new(),
        }
    }

    /// Enumerate all possible concepts
    ///
    /// ```
    /// use fca::formalconcept::*;
    ///
    /// let fc_data = 
    /// "header, att1, att2, att3, att4, att5, att6, att7
    /// obj1,     1  , 0   , 1   , 0   , 0   , 0   , 0
    /// obj2,     1  , 1   , 1   , 1   , 0   , 0   , 0
    /// obj3,     0  , 0   , 0   , 0   , 0   , 1   , 1
    /// obj4,     0  , 1   , 0   , 0   , 1   , 0   , 1";
    /// let mut fc = ConceptLattice::<String, String>::new();
    /// fc.read_csv(fc_data, b',');
    /// fc.enumerate_seq().enumerate();
    /// println!("total {} concepts enumerated.", fc.concepts.len()); 
    /// ```
    pub fn enumerate_seq (&mut self) -> SequentialConceptEnumerator<T,K>{
        SequentialConceptEnumerator::new(self)
    }
}

/// Private structure for reading csv-formal concept data
#[derive(RustcDecodable)]
struct Row {
    name: String,
    relations: Vec<u8>,
}

impl<T,K> CSVRead for ConceptLattice <T, K> 
    where T: From<String>, K: From<String> {
    fn read_csv(&mut self, content: &str, sep: u8) {
       let mut rdr = Reader::from_string(content)
                        .has_headers(true)
                        .delimiter(sep)
                        .flexible(false);

        match (rdr.headers(), rdr.decode().collect::<Result<Vec<Row>>>()) {
            (Ok(header), Ok(data)) => {
                let n_attributes = header.len() - 1;
                let n_objects = data.len();
                for item in header.into_iter().skip(1) {
                    self.attributes.push(Item::new(K::from(item.trim().to_owned()), n_objects));
                }
                for row in data.into_iter() {
                    let object_hash = self.objects.len();
                    self.objects.push(Item::new(T::from(row.name.trim().to_owned()), n_attributes));
                    let mut object = self.objects.last_mut().unwrap();
                    for (idx, relation) in row.relations.iter().enumerate() {
                        match *relation {
                            1 => {
                                self.attributes[idx].children.insert(object_hash);
                                object.children.insert(idx);
                            },
                            0 => {},
                            _ => { panic!("malformed formal concept data"); }
                        }
                    }
                }
            },
            _ => {
                panic!("can not read formal concept! {}");
            }
        }
    }
}

impl <T, K> FormalConcept for ConceptLattice<T,K> {
    fn closure_extend(&self, concept: &Concept, new_attr: &usize) -> Concept {
        let mut obj = BitVector::new(self.objects.len());
        for i in concept.extends.iter().filter(|&x| 
                       self.objects[x].children.contains(*new_attr)) {
            obj.insert(i);
        }
        let attr = self.up(&obj);
        Concept {
            extends: obj,
            intends: attr,
            parent: Vec::new(),
            children: Vec::new(),
        }

    }

    fn up(&self, obj: &BitVector) -> BitVector {
        if !obj.is_empty()  {
            let mut iter = obj.iter();
            let mut bvec = self.objects[iter.nth(0).unwrap()].children.clone(); 
            for i in iter {
                if bvec.is_empty() { break; }
                bvec.intersection_inplace(&self.objects[i].children);
            }
            bvec
        } else {
            //BitVector::ones(self.attributes.len())
            BitVector::new(0)
        }
        
    }

    fn down(&self, attr: &BitVector) -> BitVector {
        if !attr.is_empty() {
            let mut iter = attr.iter();
            let mut bvec = self.attributes[iter.nth(0).unwrap()].children.clone(); 
            for i in iter {
                if bvec.is_empty() { break; }
                bvec.intersection_inplace(&self.attributes[i].children);
            }
            bvec
        } else {
            //BitVector::ones(self.objects.len())
            BitVector::new(0)
        }
        
    }
}

/// Sequential Concept Enumerator
///
/// This is the sequential version of formal concept enumeration algorithms
/// described in 
///
/// > Parallel Recursive Algorithm for FCA
///
/// > **Petr Krajca, Jan Outrata and Vilem Vychodil**
///
/// > Concept Lattices and Their Applications (CLA), 2008.
pub struct SequentialConceptEnumerator <'a, T, K> 
    where T: 'a, K: 'a {
    cl: &'a mut ConceptLattice<T,K>,
    attr_size: usize,
}

/// Iterator implementation for iter over all concepts.
///
/// Because of the properties of DFS,
/// the enumerating status only depends on the
/// the concept and current attribute we are trying to list.
/// 
/// Thus, the basic idea is that we maintain
/// a big stack: `Vec<(Concept, usize)>` to track all the 
/// recursion. Once a new recursion occurs, we push current
/// concept and attribute, and once a new concept is found,
/// we pop it from `Vec` and return it.
/// The next time `next` is called, we can just start from
/// the last Concept-attribute pair. 
/// If `Vec` is empty, it means the enumeration is finished.
pub struct SequentialConceptIterator <'a, T, K>
    where T: 'a, K: 'a {
    cl: &'a ConceptLattice<T,K>,
    attr_size: usize,
    stack: Vec<(Concept, usize)>,
}

impl<'a, T, K> SequentialConceptIterator <'a,T,K>
    where T: 'a, K: 'a {
        fn new(cl: &'a ConceptLattice<T,K>, ) -> Self {
            let attr_size = cl.attributes.len();
            let mut stack = Vec::new();
            let attributes = BitVector::ones(cl.attributes.len());
            let objects = cl.down(&attributes);
            if objects.is_empty() {
                stack.push((Concept {
                            extends: BitVector::new(cl.objects.len()),
                            intends: attributes,
                            parent: Vec::new(), 
                            children: Vec::new(), }, attr_size));
            }
            let initial_objects = BitVector::ones(cl.objects.len());
            let initial_attributes = cl.up(&initial_objects);
            stack.push((Concept {
                extends: initial_objects,
                intends: initial_attributes,
                parent: Vec::new(), children: Vec::new(), }, 0));
            SequentialConceptIterator {
                cl: cl,
                attr_size: attr_size,
                stack: stack, 
            }
        }
}

impl<'a, T, K> Iterator for SequentialConceptIterator<'a, T, K>
    where T: 'a, K: 'a {
        type Item = Concept;

        fn next(&mut self) -> Option<Concept> {
            match self.stack.pop() {
                Some((concept, cur_attr)) => {
                    for attr in cur_attr .. self.attr_size {
                        if !concept.intends.contains(attr) {
                            let new_concept = self.cl.closure_extend(&concept, &attr);
                            if !new_concept.extends.is_empty() && !new_concept.intends.is_empty() &&
                               concept.intends.eq_left(&new_concept.intends, attr) {
                                self.stack.push((concept, attr + 1));
                                self.stack.push((new_concept, attr + 1));
                                return self.next();
                            }
                        }
                    }
                    Some(concept)
                },
                _ => None,
            }
        }
}

impl<'a, T, K> Enumerator for SequentialConceptEnumerator<'a, T, K> {
    fn enumerate(mut self) { 
        let initial_objects = BitVector::ones(self.cl.objects.len());
        let initial_attributes = self.cl.up(&initial_objects);
        self.enumerate_seq(Concept {
            extends: initial_objects,
            intends: initial_attributes,
            parent: Vec::new(), children: Vec::new(),
        }, 0);
        let attributes = BitVector::ones(self.cl.attributes.len());
        let objects = self.cl.down(&attributes);
        if objects.is_empty() {
            self.cl.concepts.push( Concept {
                        extends: BitVector::new(self.cl.objects.len()),
                        intends: attributes,
                        parent: Vec::new(), 
                        children: Vec::new(), }
                )
        }
    }
}

impl <'a, T, K> SequentialConceptEnumerator <'a, T, K>
    where T: 'a, K: 'a {
    pub fn new(cl: &'a mut ConceptLattice<T,K>) -> Self {
        let size = cl.attributes.len();
        SequentialConceptEnumerator {
            cl: cl, attr_size: size,
        }
    }

    pub fn iter(&'a self) -> SequentialConceptIterator<'a, T, K> {
        SequentialConceptIterator::new(self.cl)
    }

    fn enumerate_seq(&mut self, concept: Concept, cur_attr: usize) {
        for attr in cur_attr .. self.attr_size {
            if !concept.intends.contains(attr) {
                let new_concept = self.cl.closure_extend(&concept, &attr);
                if !new_concept.extends.is_empty() && !new_concept.intends.is_empty() &&
                   concept.intends.eq_left(&new_concept.intends, attr) {
                    self.enumerate_seq(new_concept, attr + 1);
                }
            }
        }
        if cfg!(enumeration = "dummy") {
            self.cl.concepts.push(
                Concept{
                    intends: BitVector::new(0), 
                    extends: BitVector::new(0),
                    parent: Vec::new(), 
                    children: Vec::new()});
        } else {
            self.cl.concepts.push(concept);
        }
    }
}

/// The basic struct of a concept
#[derive(Clone)]
pub struct Concept {
    /// attributes
    intends: BitVector,
    /// object
    extends: BitVector,

    parent: Vec<usize>,
    children: Vec<usize>,
}

impl fmt::Debug for Concept {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "[intends: {}, ", self.intends));
        write!(f, "extends: {} ]", self.extends)
    }
}

impl fmt::Display for Concept {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self, f)
    }
}

/// An user-defined concept item
pub struct Item <T> {
    pub item: T,
    pub children: BitVector,
}

impl<T> Item<T> {
    pub fn new(item: T, len: usize) -> Self {
        Item {
            item: item,
            children: BitVector::new(len),
        }
    }
}

impl<T> fmt::Debug for Item<T> 
    where T: fmt::Debug + fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Item: {:?}, Attributes: {}", self.item, self.children)
    }
}

impl<T> fmt::Display for Item<T> 
    where T: fmt::Debug + fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Item: {}, Attributes: {}", self.item, self.children)
    }
}

