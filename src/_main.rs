#![allow(dead_code)]

use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display};
use std::mem;
use num::{PrimInt, ToPrimitive, Unsigned};

trait Bits: Debug + PrimInt + Unsigned {
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

#[derive(Debug)]
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
        // TODO check arg_vals and args same len
        arg_vals.iter()
            .zip(self.args.iter())
            .map(| pair | (*pair.1).eat(*pair.0))
            .fold(self.base, | acc, x | acc | x)
    }

    fn arg_count(&self) -> u32 {
        self.args.len() as u32
    }
}

// instructions are used because
//   for jumps you need to know the location of all the opdefs
//     before you generate the code for them

#[derive(Debug)]
struct Idef<T: Bits> {
    name: String,
    opdef: Opdef<T>,
    shifted: bool,
}

impl<T: Bits> Idef<T> {
    fn apply(&self, args: &[u64]) -> T {
        if self.shifted {
            let args: Vec<T> = args.iter()
                                   .map(| &arg | T::from_u64(arg))
                                   .collect();
            self.opdef.apply(&args)
        } else {
            let arg = T::from_u64(args[0] >> mem::size_of::<T>() * 8);
            self.opdef.apply(&[arg])
        }
    }
}

//

#[derive(Copy, Clone, Debug)]
enum HexRecordType {
    Data,
    EndOfFile,
    ExtendedSegmentAddress,
    StartSegmentAddress,
    ExtendedLinearAddress,
    StarLinearAddress,
}

#[derive(Debug)]
struct HexRecord {
    typ: HexRecordType,
    addr: u16,
    data: Vec<u8>,
}

impl HexRecord {
    fn checksum(&self) -> u8 {
        fn twos_complement(num: u16) -> u16 {
            (num ^ u16::max_value()).wrapping_add(1)
        }

        let mut ret: u16 = 0;

        ret = ret.wrapping_add(self.addr);
        ret = ret.wrapping_add(self.typ as u16);
        ret = ret.wrapping_add(self.addr >> 8);
        ret = ret.wrapping_add(self.data.len() as u16);
        for byte in &self.data {
            ret = ret.wrapping_add(*byte as u16)
        }

        (0xff & twos_complement(ret)) as u8
    }
}

impl Display for HexRecord {
    fn fmt(&self, f: &mut fmt::Formatter)-> fmt::Result {
        write!(f, ":{:02x}{:04x}{:02x}"
             , self.data.len() & 0xff
             , self.addr
             , self.typ as u8)?;
        for byte in &self.data {
            write!(f, "{:02x}", *byte)?;
        }
        write!(f, "{:02x}", self.checksum())
    }
}

//

#[derive(Debug)]
enum IArg {
    Raw(u64),
    LabelAccess {
        name: String,
        is_relative: bool,
        offset: i32,
    }
}

#[derive(Debug)]
struct Instruction<'a, T: Bits> {
    idef: &'a Idef<T>,
    args: Vec<IArg>,
}

#[derive(Copy, Clone, Debug)]
struct AddressTag<A: Bits> {
    addr: A
}

#[derive(Debug)]
struct LabelTag {
    name: String,
}

#[derive(Debug)]
enum CodeObject<'a, T: Bits, A: Bits> {
    Instruction(Instruction<'a, T>),
    RawData(T),
    AddressTag(AddressTag<A>),
    LabelTag(LabelTag),
}

//

#[derive(Debug)]
struct CompileSettings {
    eof_record: HexRecord,
    words_per_record: u8,
}

#[derive(Debug)]
enum CompileError {
    StartWithAddressTag,
    DuplicateLabel(String),
}

#[derive(Debug)]
struct Code<'a, T: Bits, A: Bits> {
    code: Vec<CodeObject<'a, T, A>>,
    addr_image: Vec<A>,
    label_table: HashMap<String, A>,
}

// TODO check that code.len() doesnt wrapover type of A
//       for the A::from(i).unwrap()
impl<'a, T: Bits, A: Bits> Code<'a, T, A> {
    fn new(code: Vec<CodeObject<'a, T, A>>) -> Result<Code<'a, T, A>, CompileError> {
        let mut offset: A =
            if let CodeObject::AddressTag(addr_tag) = code[0] {
                addr_tag.addr
            } else {
                return Err(CompileError::StartWithAddressTag);
            };

        let mut addr_image = Vec::new();
        addr_image.reserve(code.len());

        for i in 0..code.len() {
            addr_image.push(A::from(offset).unwrap());
            match code[i] {
                CodeObject::AddressTag(addr_tag) => {
                    offset = addr_tag.addr;
                },
                CodeObject::RawData(_) | CodeObject::Instruction(_) => {
                    offset = offset + A::one();
                },
                _ => {}
            }
        }

        let addr_image = addr_image;
        let mut label_table = HashMap::new();

        for (&addr, obj) in addr_image.iter().zip(code.iter()) {
            if let CodeObject::LabelTag(label_tag) = obj {
                if label_table.contains_key(&label_tag.name) {
                    return Err(CompileError::DuplicateLabel(label_tag.name.clone()));
                }
                label_table.insert(label_tag.name.clone(), addr);
            }
        }

        Ok(Code {
            code,
            addr_image,
            label_table,
        })
    }
}

//

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

    let idef_add = Idef {
        name: "add".to_string(),
        opdef: Opdef::<u8>::new("0011aabb", "ab"),
        shifted: false,

    };
    let idef_jmp = Idef {
        name: "jmp".to_string(),
        opdef: Opdef::<u8>::new("0011aaaa", "a"),
        shifted: true,
    };

    let c = Code::new(vec![
        CodeObject::AddressTag(AddressTag{addr: 0u16}),
        CodeObject::LabelTag(LabelTag{name: "start".to_string()}),
        CodeObject::Instruction(Instruction{idef: &idef_add, args: vec![IArg::Raw(0b11), IArg::Raw(0b00)]}),
        CodeObject::Instruction(Instruction{idef: &idef_jmp, args: vec![IArg::Raw(0xdead)]}),
        CodeObject::RawData(0xad),
    ]);

    println!("{:?}", c);

    let c = Code::new(vec![
        CodeObject::AddressTag(AddressTag{addr: 0u16}),
        CodeObject::LabelTag(LabelTag{name: "start".to_string()}),
        CodeObject::Instruction(Instruction{idef: &idef_add, args: vec![IArg::Raw(0b11), IArg::Raw(0b00)]}),
        CodeObject::Instruction(Instruction{idef: &idef_jmp, args: vec![IArg::Raw(0xdead)]}),
        CodeObject::LabelTag(LabelTag{name: "another".to_string()}),
        CodeObject::RawData(0xad),
    ]);

    println!("{:?}", c);

    /*
    println!("{:08b}", idef_add.apply(&[0b10, 0b11])[0]);
    let vals = idef_jmp.apply(&[0b0000100011001110]);
    println!("{:08b} {:08b}", vals[0], vals[1]);

    let hr = HexRecord {
        typ: HexRecordType::Data,
        addr: 0x0030,
        data: vec![
            0x02,
            0x33,
            0x7a,
        ]
    };
    println!("{}", hr);
    */
}
