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
        let mut ret = vec![0u8; ct];
        for i in (0..ct).rev() {
            ret[i] = (self & Self::from(0xff).unwrap()).to_u8().unwrap();
            self = self >> 8;
        }
        ret
    }
}

// impl Bits for u8 {}
// impl Bits for u16 {}
// impl Bits for u32 {}
impl Bits for u64 {}

//

#[derive(Debug)]
struct Opdef {
    name: String,
    base: u64,
    args: Vec<u64>,
    shift: i32,
}

impl Opdef {
    fn new(name: &str, spec_str: &str, arg_order: &str, shift: i32) -> Self {
        let spec_bytes = spec_str.as_bytes();
        let arg_bytes = arg_order.as_bytes();

        let base = spec_bytes.iter()
            .map(| &byte | byte == '1' as u8)
            .fold(0, | acc, tf | {
                if tf {
                    (acc << 1) | 1
                } else {
                    acc << 1
                }
            });

        let args = arg_bytes.iter()
            .map(| &arg_byte | {
                spec_bytes.iter()
                    .map(| &spec_byte | arg_byte == spec_byte)
                    .fold(0, | acc, tf | {
                        if tf {
                            (acc << 1) | 1
                        } else {
                            acc << 1
                        }
                    })
            })
            .collect();

        Opdef {
            name: name.to_string(),
            base,
            args,
            shift,
        }
    }

    fn apply(&self, arg_vals: &[u64]) -> u64 {
        // TODO check arg_vals and args same len
        if self.shift == 0 {
            self.args.iter()
                .zip(arg_vals.iter())
                .map(| (&mask, &arg) | mask.eat(arg))
                .fold(self.base, | acc, x | acc | x)
        } else {
            let arg = arg_vals[0] >> self.shift;
            self.base | self.args[0].eat(arg)
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

#[derive(Clone, Debug)]
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

        ret = ret.wrapping_add(self.data.len() as u16);
        ret = ret.wrapping_add(self.addr);
        ret = ret.wrapping_add(self.addr >> 8);
        ret = ret.wrapping_add(self.typ as u16);
        for &byte in &self.data {
            ret = ret.wrapping_add(byte as u16)
        }

        (0xff & twos_complement(ret)) as u8
    }
}

impl Display for HexRecord {
    fn fmt(&self, f: &mut fmt::Formatter)-> fmt::Result {
        write!(f, ":{:02x}{:04x}{:02x}",
               self.data.len() & 0xff,
               self.addr,
               self.typ as u8)?;
        for byte in &self.data {
            write!(f, "{:02x}", *byte)?;
        }
        write!(f, "{:02x}", self.checksum())
    }
}

//

#[derive(Clone, Debug)]
enum IArg {
    Raw(u64),
    LabelAccess {
        name: String,
        is_relative: bool,
        offset: i32,
    }
}

impl IArg {
    fn resolve(&self, addr: u64, label_table: &HashMap<String, u64>) -> Result<u64, CompileError> {
        match self {
            IArg::Raw(val) => Ok(*val),
            IArg::LabelAccess {
                name,
                is_relative,
                offset
            } => {
                if let Some(label_addr) = label_table.get(name) {
                    if *is_relative {
                        // TODO check this works
                        Ok((*label_addr as i32
                            - addr as i32
                            + *offset) as u64)
                    } else {
                        Ok(*label_addr)
                    }
                } else {
                    Err(CompileError::LabelNotFound(name.clone()))
                }
            },
        }
    }
}

#[derive(Debug)]
struct Instruction<'a> {
    opdef: &'a Opdef,
    args: Vec<IArg>,
}

//

#[derive(Debug)]
enum CodeObject<'a> {
    Instruction(Instruction<'a>),
    AddressTag(u64),
    LabelTag(String),
}

#[derive(Debug)]
enum HexObject {
    AddressTag(u64),
    Opcode(u64),
}

// todo default
#[derive(Clone, Debug)]
struct CompileSettings {
    opcode_size: u8,
    address_size: u8,
    eof_record: HexRecord,
    words_per_record: u8,
}

#[derive(Debug)]
enum CompileError {
    StartWithAddressTag,
    DuplicateLabel(String),
    LabelNotFound(String),
}

fn generate_address_image(code: &[CodeObject]) -> Result<Vec<u64>, CompileError> {
    use CodeObject::*;

    let mut offset =
        if let AddressTag(addr) = code[0] {
            addr
        } else {
            return Err(CompileError::StartWithAddressTag);
        };

    let mut addr_image = Vec::new();
    addr_image.reserve(code.len());

    for obj in code.iter() {
        addr_image.push(offset);
        match obj {
            AddressTag(addr) => offset = *addr,
            Instruction{..}  => offset += 1,
            _ => {}
        }
    }

    Ok(addr_image)
}

fn generate_label_table(code: &[CodeObject], address_image: &[u64]) -> Result<HashMap<String, u64>, CompileError> {
    use CodeObject::*;

    let mut label_table = HashMap::new();

    for (obj, addr) in code.iter().zip(address_image.iter()) {
        match obj {
            LabelTag(label) => {
                let name = label.clone();
                if label_table.contains_key(label) {
                    return Err(CompileError::DuplicateLabel(name));
                } else {
                    label_table.insert(name, *addr);
                }
            }
            _ => {}
        }
    }

    Ok(label_table)
}

fn generate_hex_objects(
    code: &[CodeObject],
    address_image: &[u64],
    label_table: &HashMap<String, u64>
) -> Result<Vec<HexObject>, CompileError> {
    use CodeObject::*;

    code.iter()
        .zip(address_image.iter())
        .filter(| (obj, _) | !matches!(obj, LabelTag(_)))
        .map(| (obj, &addr) | {
            match obj {
                AddressTag(tag_addr) => Ok(HexObject::AddressTag(*tag_addr)),
                Instruction(inst)    => {
                    let args: Result<Vec<_>, _> =
                        inst.args.iter()
                                 .map(| arg | arg.resolve(addr, &label_table))
                                 .collect();
                    match args {
                        Ok(args) => Ok(HexObject::Opcode(inst.opdef.apply(&args))),
                        Err(err) => Err(err),
                    }
                }
                _ => unreachable!(),
            }
        })
        .collect()
}

fn generate_hex_records(hex_objects: &[HexObject], settings: &CompileSettings) -> Result<Vec<HexRecord>, CompileError> {
    let mut records = Vec::new();

    let i_addresses = hex_objects.iter()
                                 .filter_map(| obj | {
                                     if let HexObject::AddressTag(addr) = obj {
                                         Some(addr)
                                     } else {
                                         None
                                     }
                                 });

    let i_splits = hex_objects.split(| obj | matches!(obj, HexObject::AddressTag(_)));

    // TODO
    // note, i_splits skips the first one
    for (split_addr, split) in i_addresses.zip(i_splits.skip(1)) {
        let i_chunks = split.chunks(settings.words_per_record as usize);
        for (chunk_addr, chunk) in i_chunks.enumerate() {
            records.push(HexRecord {
                typ: HexRecordType::Data,
                // TODO check somehow?
                addr: (split_addr + (chunk_addr * settings.words_per_record as usize) as u64) as u16,
                data: chunk.iter()
                           .flat_map(| obj | {
                               let opcode =
                                   match obj {
                                       HexObject::Opcode(val) => val,
                                       _ => unreachable!(),
                                   };

                               opcode.to_bytes(settings.opcode_size as usize)
                           })
                           .collect(),
            });
        }
    }

    records.push(settings.eof_record.clone());

    Ok(records)
}

fn compile(settings: CompileSettings, code: Vec<CodeObject>) -> Result<Vec<HexRecord>, CompileError> {
    let address_image = generate_address_image(&code)?;
    let label_table = generate_label_table(&code, &address_image)?;
    let hex_objects = generate_hex_objects(&code, &address_image, &label_table)?;
    generate_hex_records(&hex_objects, &settings)
}

//

fn main() {
    let opdef_add = Opdef::new("add", "0011aabb", "ab", 0);
    let opdef_jmp = Opdef::new("jmp", "1100aaaa", "a", 8);
    let opdef_direct = Opdef::new("direct", "aaaaaaaa", "a", 0);

    let settings = CompileSettings {
        opcode_size: 2,
        address_size: 2,
        eof_record: HexRecord {
            typ: HexRecordType::EndOfFile,
            data: Vec::new(),
            addr: 0,
        },
        words_per_record: 10
    };

    let lb = IArg::LabelAccess {
        name: "start".to_string(),
        is_relative: false,
        offset: 0,
    };

    let c =
        vec![
            CodeObject::AddressTag(0xdead),
            CodeObject::LabelTag("start".to_string()),
            CodeObject::Instruction(Instruction { opdef: &opdef_add, args: vec![IArg::Raw(0b11), IArg::Raw(0b00)]} ),
            CodeObject::Instruction(Instruction { opdef: &opdef_add, args: vec![IArg::Raw(0b11), IArg::Raw(0b00)]} ),
            CodeObject::Instruction(Instruction { opdef: &opdef_add, args: vec![IArg::Raw(0b11), IArg::Raw(0b00)]} ),
            CodeObject::Instruction(Instruction { opdef: &opdef_add, args: vec![IArg::Raw(0b11), IArg::Raw(0b00)]} ),
            CodeObject::Instruction(Instruction { opdef: &opdef_add, args: vec![IArg::Raw(0b11), IArg::Raw(0b00)]} ),
            CodeObject::Instruction(Instruction { opdef: &opdef_add, args: vec![IArg::Raw(0b11), IArg::Raw(0b00)]} ),
            CodeObject::AddressTag(0xdeac),
            CodeObject::LabelTag("another".to_string()),
            CodeObject::Instruction(Instruction { opdef: &opdef_jmp, args: vec![lb.clone()]} ),
            CodeObject::Instruction(Instruction { opdef: &opdef_direct, args: vec![lb]} ),
            CodeObject::Instruction(Instruction { opdef: &opdef_jmp, args: vec![IArg::Raw(0xdead)]} ),
            CodeObject::Instruction(Instruction { opdef: &opdef_direct, args: vec![IArg::Raw(0xdead)]} ),
            CodeObject::Instruction(Instruction { opdef: &opdef_jmp, args: vec![IArg::Raw(0xdead)]} ),
            CodeObject::Instruction(Instruction { opdef: &opdef_direct, args: vec![IArg::Raw(0xdead)]} ),
        ];

    let hrs = compile(settings, c).unwrap();
    for hr in hrs {
        println!("{}", hr);
    }
}
