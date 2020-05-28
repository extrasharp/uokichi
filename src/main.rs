use std::fmt::{Debug};
use std::mem;
use num::{PrimInt, ToPrimitive};

trait Bits: 'static + Debug + PrimInt + Unsigned {
    // has to be unsigned
    //   1. using them as bytes anyway
    //   2. max_value is all ones

    fn from_u64(val: u64) -> Self {
       Self::from(val & Self::max_value().to_u64().unwrap()).unwrap()
    }

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

#[derive(Copy, Clone)]
enum IdefType {
    Simple,
    Shifted,
}

struct Idef<T: Bits> {
    name: String,
    opdef: Opdef<T>,
    typ: IdefType,
    word_count: u8,
}

impl<T: Bits> Idef<T> {
    fn simple(name: &str, opdef: Opdef<T>) -> Idef<T> {
        Idef {
            name: String::from(name),
            opdef,
            typ: IdefType::Simple,
            word_count: 1,
        }
    }

    fn shifted(name: &str, opdef: Opdef<T>) -> Idef<T> {
        Idef {
            name: String::from(name),
            opdef,
            typ: IdefType::Shifted,
            word_count: 2,
        }
    }

    fn apply(&self, args: &[u64]) -> Vec<T> {
        match self.typ {
            IdefType::Simple => {
                let args: Vec<T> = args.iter()
                                       .map(| &arg | T::from_u64(arg))
                                       .collect();
                let mut ret = Vec::new();
                ret.push(self.opdef.apply(&args));
                ret
            },
            IdefType::Shifted => {
                let arg = args[0];
                let mut ret = Vec::new();
                ret.push(self.opdef.apply(&[T::from_u64(arg >> mem::size_of::<T>() * 8)]));
                ret.push(T::from_u64(arg));
                ret
            },
        }
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

    let idef_add = Idef::simple("add", Opdef::<u8>::new("0011aabb", "ab"));
    let idef_jmp = Idef::shifted("jmp", Opdef::<u8>::new("0011aaaa", "a"));

    println!("{:08b}", idef_add.apply(&[0b10, 0b11])[0]);
    let vals = idef_jmp.apply(&[0b0000100011001110]);
    println!("{:08b} {:08b}", vals[0], vals[1]);
}
