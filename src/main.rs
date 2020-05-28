use std::mem;
use num::{PrimInt, ToPrimitive};

trait Bits: PrimInt + 'static {
    fn mask(ct: usize) -> Self {
        !(Self::max_value() << ct)
    }

    fn is_bit_set(self, at: usize) -> bool {
        self >> at & Self::one() == Self::one()
    }

    fn eat(mut self, mut val: Self) -> Self {
        let bit_sz = mem::size_of::<Self>() * 8;
        let mut ret = Self::zero();
        for i in 0..bit_sz {
            if self & Self::one() == Self::one() {
                ret = ret | ((val & Self::one()) << i);
                val = val >> 1;
            }
            self = self >> 1;
        }
        ret
    }

    fn to_bytes(mut self, ct: usize) -> Vec<u8>
    where
        Self: ToPrimitive {
        let mut ret = vec![0; ct];
        for i in (0..ct).rev() {
            ret[i] = (self & Self::from(0xff).unwrap()).to_u8().unwrap();
            self = self >> 8;
        }
        ret
    }
}

// has to be unsigned
//   1. using them as bytes
//   2. max_value is all ones
impl Bits for u8 {}
impl Bits for u16 {}
impl Bits for u32 {}
impl Bits for u64 {}

//

struct Opdef<T: Bits> {
    base: T,
    args: Vec<T>
}

impl<T: Bits> Opdef<T> {
    fn new(spec_str: &str, arg_order: &str) -> Self {
        let spec_bytes = spec_str.as_bytes();
        let arg_bytes = arg_order.as_bytes();

        let base = spec_bytes.iter()
            .fold(T::zero(), | acc, byte | {
                if *byte == '1' as u8 {
                    (acc << 1) | T::one()
                } else {
                    acc << 1
                }
            });

        let args = arg_bytes.iter()
            .map(| arg_byte | {
                spec_bytes.iter()
                    .fold(T::zero(), | acc, spec_byte | {
                        if *arg_byte == *spec_byte {
                            (acc << 1) | T::one()
                        } else {
                            acc << 1
                        }
                    })
            })
            .collect();

        Opdef {
            base,
            args,
        }
    }

    fn apply(&self, arg_vals: &[T]) -> T {
        arg_vals.iter()
            .zip(self.args.iter())
            .map(| pair | {
                (*pair.1).eat(*pair.0)
            })
            .fold(self.base, | acc, x | acc | x)
    }

    fn arg_count(&self) -> u32 {
        self.args.len() as u32
    }
}

// instructions are used because
//   for jumps you need to know the location of all the opdefs
//     before you generate the code for them

struct Idef<T: Bits> {
    name: String,
    opdefs: Vec<Opdef<T>>,
    arg_shifts: Vec<i8>,
}

impl<T: Bits> Idef<T> {
    fn apply(&self, args: &[u64]) -> Vec<T> {
        let args: Vec<T> = args.iter()
                               .zip(self.arg_shifts.iter())
                               .map(| pair | {
                                   T::from((pair.0 << pair.1)
                                           & T::max_value().to_u64().unwrap())
                                       .unwrap()
                               })
                       .collect();
        vec![self.opdefs[0].apply(&args); 1]
    }
}

enum IArg {
    Raw(u64),
    LabelAccess {
        name: String,
        is_relative: bool,
        offset: i32,
    }
}

struct Instruction<T: Bits> {
    idef: &'static Idef<T>,
    word_count: u32,
    args: Vec<IArg>,
}


// could supply args with a function ptr
// Fn(ctx) -> arg: u32
//   ctx has like curr add and label table

/*
struct Context {
}

enum OpArg {
    Flat(u32)
    Closure {
      Fn() -> u32,
    }
}

struct Opcode {
    def: OpDef
    arg: OpArg
}

*/
/*

enum Applier<T> {
    Simple(opd: Opdef<T>),
}

struct IDef<T: Bits> {
    name: String,
    arg_count: u32,
    word_count: u32,
    applier: Applier,
}

impl<T: Bits, U: Apply> for IDef<T, U> {
    fn name(&self) -> &String {
        &self.name
    }

    fn arg_count(&self) -> u32 {
        self.arg_count
    }

    fn word_count(&self) -> u32 {
        self.word_count
    }

    fn apply(&self, args: &[u32]) -> Vec<T> {
        self.apply_data.apply()
    }
}
*/
/*

struct IDef<T: Bits> {
    name: String,
    arg_count: u32,
    word_count: u32,
    apply_fn: Box<dyn FnOnce(&[u32]) -> Vec<T>>
}

impl<T: Bits> IDef<T> {
    fn simple(name: &str, opd: Opdef<T>) -> IDef<T> {
        IDef {
            name: String::from(name),
            arg_count: opd.arg_count(),
            word_count: 1,
            apply_fn: Box::new(| args: &[u32] | -> Vec<T> {
                let opd = opd;
                let args: Vec<T> = args.iter()
                                       .map(| arg | T::from(*arg).unwrap())
                                       .collect();
                vec![opd.apply(&args); 1]
            })
        }
    }
    fn name(&self) -> &String {

        &self.name
    }

    fn arg_count(&self) -> u32 {
        self.arg_count
    }

    fn word_count(&self) -> u32 {
        self.word_count
    }

    fn apply(&self, args: &[u32]) -> Vec<T> {
        (&self.apply_fn)(args)
    }
}
*/

//

/*
trait IDef<T: Bits> {
    fn name(&self) -> &String;
    fn arg_count(&self) -> u32;
    fn word_count(&self) -> u32;
    fn apply(&self, args: &[T]) -> Vec<T>;
}

struct SimpleIDef<T: Bits> {
    name: String,
    arg_count: u32,
    word_count: u32,
    opd: Opdef<T>,
}

impl<T: Bits> SimpleIDef<T> {
    fn new(name: &str, opd: Opdef<T>) -> SimpleIDef<T> {
        SimpleIDef {
            name: String::from(name),
            arg_count: opd.arg_count(),
            word_count: 1,
            opd,
        }
    }
}

impl<T: Bits> IDef<T> for SimpleIDef<T> {
    fn name(&self) -> &String {
        &self.name
    }

    fn arg_count(&self) -> u32 {
        self.arg_count
    }

    fn word_count(&self) -> u32 {
        self.word_count
    }

    fn apply(&self, args: &[T]) -> Vec<T> {
        vec![self.opd.apply(args); 1]
    }
}

struct MultipleIDef<T: Bits> {
    name: String,
    arg_count: u32,
    word_count: u32,
    opds: Vec<Opdef<T>>,
}

impl<T: Bits> MultipleIDef<T> {
    fn new(name: &str, opds: Vec<Opdef<T>>) -> MultipleIDef<T> {
        MultipleIDef {
            name: String::from(name),
            arg_count: opds.iter()
                       .fold(0, | acc, opd | acc + opd.arg_count()),
            word_count: opds.len() as u32,
            opds,
        }
    }
}

impl<T: Bits> IDef<T> for MultipleIDef<T> {
    fn name(&self) -> &String {
        &self.name
    }

    fn arg_count(&self) -> u32 {
        self.arg_count
    }

    fn word_count(&self) -> u32 {
        self.word_count
    }

    fn apply(&self, args: &[T]) -> Vec<T> {
        // TODO
        let mut ret = Vec::<T>::new();
        // ret.push(self.opd.apply(args));
        ret
    }
}

struct ShiftedIDef<T: Bits> {
    name: String,
    arg_count: u32,
    word_count: u32,
    opd: Opdef<T>,
    size: u32,
}

impl<T: Bits> ShiftedIDef<T> {
    fn new(name: &str, opd: Opdef<T>, size: u32) -> ShiftedIDef<T> {
        ShiftedIDef {
            name: String::from(name),
            arg_count: 1,
            word_count: 2,
            opd,
            size,
        }
    }
}

impl<T: Bits> IDef<T> for ShiftedIDef<T> {
    fn name(&self) -> &String {
        &self.name
    }

    fn arg_count(&self) -> u32 {
        self.arg_count
    }

    fn word_count(&self) -> u32 {
        self.word_count
    }

    // note: args need to be longer then T
    // being a u32 or u64 is fine
    fn apply(&self, args: &[T]) -> Vec<T> {
        let mut ret = Vec::<T>::new();
        // ret.push(self.opd.apply(args));
        ret
    }
}

struct Instruction<T, U: IDef> {
    idef: U,
    // args:
}
*/

fn main() {
    // let k: u32 = 0b01010011.eat(0b0101);
    // let k = u8::mask(3);
    // let f = 0xdeadbeefu32.to_bytes(8);
    // println!("0b{:08b}", k);
    // println!("{:?}", f);
    // println!("{} {} {} {}", 0xde, 0xad, 0xbe, 0xef);

    // let opd = Opdef::<u8>::new("0101aabb", "ab");
    // println!("{:08b}", opd.apply(&[0b11, 0b01]));
    // println!("{:08b}", opd.apply(&[0b01, 0b11]));
    // println!("{:08b}", opd.apply(&[0b10, 0b00]));

    // let idef = IDef::simple("add", Opdef::<u8>::new("1010aabb", "ab"));
    let idef_add = Idef {
        name: String::from("add"),
        opdefs: vec![Opdef::<u8>::new("aabb1100", "ab")],
        arg_shifts: vec![0; 2]
    };

    println!("{:08b}", idef_add.apply(&[0b10, 0b11])[0])
}
