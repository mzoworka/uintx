use bitenum::{BitEnum, BitEnumTrait};
use int_enum::IntEnum;
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug)]
pub struct Error {
    pub value: u8,
    pub type_name: &'static str,
    pub text: &'static str,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "uintx error({}): {}, value: {}",
            self.type_name, self.text, self.value
        )
    }
}

impl From<bincode::error::DecodeError> for Error {
    fn from(value: bincode::error::DecodeError) -> Self {
        let (val, type_name, text) = match value {
            bincode::error::DecodeError::UnexpectedEnd { additional } => {
                (additional as u8, "DecodeError", "UnexpectedEnd")
            }
            bincode::error::DecodeError::LimitExceeded => (0u8, "DecodeError", "LimitExceeded"),
            bincode::error::DecodeError::InvalidIntegerType {
                expected: _,
                found: _,
            } => (0u8, "DecodeError", "InvalidIntegerType"),
            bincode::error::DecodeError::NonZeroTypeIsZero { non_zero_type: _ } => {
                (0u8, "DecodeError", "NonZeroTypeIsZero")
            }
            bincode::error::DecodeError::UnexpectedVariant {
                type_name,
                allowed: _,
                found,
            } => (found as u8, type_name, "UnexpectedVariant"),
            bincode::error::DecodeError::Utf8 { inner: _ } => (0u8, "DecodeError", "Utf8"),
            bincode::error::DecodeError::InvalidCharEncoding(_) => {
                (0u8, "DecodeError", "InvalidCharEncoding")
            }
            bincode::error::DecodeError::InvalidBooleanValue(_) => {
                (0u8, "DecodeError", "InvalidBooleanValue")
            }
            bincode::error::DecodeError::ArrayLengthMismatch { required: _, found } => {
                (found as u8, "DecodeError", "ArrayLengthMismatch")
            }
            bincode::error::DecodeError::OutsideUsizeRange(_) => {
                (0u8, "DecodeError", "OutsideUsizeRange")
            }
            bincode::error::DecodeError::EmptyEnum { type_name } => (0u8, type_name, "EmptyEnum"),
            bincode::error::DecodeError::InvalidDuration { secs: _, nanos: _ } => {
                (0u8, "DecodeError", "InvalidDuration")
            }
            bincode::error::DecodeError::InvalidSystemTime { duration: _ } => {
                (0u8, "DecodeError", "InvalidSystemTime")
            }
            bincode::error::DecodeError::CStringNulError { position } => {
                (position as u8, "DecodeError", "CStringNulError")
            }
            bincode::error::DecodeError::Io {
                inner: _,
                additional,
            } => (additional as u8, "DecodeError", "Io"),
            bincode::error::DecodeError::Other(text) => (0u8, "DecodeError", text),
            bincode::error::DecodeError::OtherString(_) => (0u8, "DecodeError", "OtherString"),
            _ => (0u8, "DecodeError", "unknown"),
        };
        Self {
            value: val,
            type_name,
            text,
        }
    }
}

pub const fn _f_bit_guard<const BITS: u8>() -> bool {
    if !(BITS <= 8 && BITS > 0) {
        panic!("guard evaluated to false")
    }
    true
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct UintX<const BITS: u8>(pub u8);

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct EnumUIntX<T, const BITS: u8>(pub T);

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct BitEnumUIntX<T, const BITS: u8>(pub BitEnum<T>)
where
    T: Sized + int_enum::IntEnum,
    <T as int_enum::IntEnum>::Int: Default + for<'a> Deserialize<'a> + Serialize;

pub trait BitData
where
    Self: Sized,
{
    const BITS: u8;
    fn from_u8(data: u8) -> Result<Self, Error>;
    fn get(&self) -> u8;
}

impl<const BITS: u8> BitData for UintX<BITS>
{
    const BITS: u8 = BITS;

    fn from_u8(data: u8) -> Result<Self, Error> {
        Ok(Self(data))
    }

    fn get(&self) -> u8 {
        self.0
    }
}

impl<T, const BITS: u8> BitData for EnumUIntX<T, BITS>
where
    T: IntEnum,
    <T as IntEnum>::Int: num::NumCast,
    <T as IntEnum>::Int: Default,
{
    const BITS: u8 = BITS;

    fn from_u8(data: u8) -> Result<Self, Error> {
        T::from_int(num::cast(data).unwrap_or_default())
            .map_err(|_e| Error {
                value: data,
                type_name: std::any::type_name::<T>(),
                text: "unknown variant",
            })
            .map(|x| Self(x))
    }

    fn get(&self) -> u8 {
        num::cast(self.0.int_value()).unwrap_or_default()
    }
}

impl<T, const BITS: u8> BitData for BitEnumUIntX<T, BITS>
where
    T: Sized + int_enum::IntEnum,
    <T as int_enum::IntEnum>::Int: Default + for<'a> Deserialize<'a> + Serialize,
    <T as IntEnum>::Int: num::NumCast,
{
    const BITS: u8 = BITS;

    fn from_u8(data: u8) -> Result<Self, Error> {
        Ok(Self(
            BitEnum::<T>::from_int(num::cast(data).ok_or(Error {
                value: data,
                type_name: std::any::type_name::<T>(),
                text: "cannot cast u8 data",
            })?)
            .map_err(|_e| Error {
                value: data,
                type_name: std::any::type_name::<T>(),
                text: "unknown variant",
            })?,
        ))
    }

    fn get(&self) -> u8 {
        num::cast(self.0.get_val()).unwrap_or_default()
    }
}

pub trait BitEncode
where
    Self: Sized,
{
    const BITS: u8;

    fn encode(&self) -> Result<Vec<u8>, Error>;
    fn decode(data: &[u8]) -> Result<Self, Error>;
    fn decode_from_reader<R: bincode::de::read::Reader>(reader: &mut R) -> Result<Self, Error>
    where
        [(); Self::BITS as usize / 8]:,
    {
        let mut data = [0u8; Self::BITS as usize / 8];
        R::read(reader, &mut data).map_err(<bincode::error::DecodeError as Into<Error>>::into)?;
        Self::decode(&data)
    }
}

fn from_buf(buf: &[u8], start: usize, size: usize) -> u8 {
    let byte_offset = start / 8;
    let bit_offset = start % 8;
    let end = bit_offset + size;
    let mut b1 = buf[byte_offset] & (0xFF >> (bit_offset));
    let mut b2 = 0;
    if end <= 8 {
        b1 >>= 8 - end;
    } else {
        b1 <<= size - (16 - end);
        b2 = buf[byte_offset + 1] >> (16 - end);
    }

    b1 | b2
}

fn to_buf(buf: &mut [u8], data: u8, offset: usize, size: usize) -> usize {
    let current_offset_end = size + (offset % 8);
    let val = match current_offset_end {
        x if x <= 8 => (data << (8 - x), None), //fits in the same byte
        x if x > 8 => (
            data >> (x - 8),
            Some((data & (0xFF >> (x - 8))) << (16 - x)),
        ),
        _ => panic!("not possible"),
    };

    buf[offset / 8] |= val.0;
    if let Some(x) = val.1 {
        buf[(offset / 8) + 1] |= x;
    }

    current_offset_end
}

impl<T1> BitEncode for (T1,)
where
    T1: BitData,
    [(); (T1::BITS) as usize / 8]:,
{
    const BITS: u8 = T1::BITS;

    fn encode(&self) -> Result<Vec<u8>, Error> {
        let mut buf: [u8; (T1::BITS) as usize / 8] = [0; (T1::BITS) as usize / 8];
        to_buf(&mut buf, self.0.get(), 0, (T1::BITS) as usize);

        Ok(buf.into_iter().collect::<Vec<_>>())
    }

    fn decode(data: &[u8]) -> Result<Self, Error>
    where
        Self: Sized,
    {
        Ok((T1::from_u8(from_buf(data, 0, (T1::BITS) as usize))?,))
    }
}

impl<T1, T2> BitEncode for (T1, T2)
where
    T1: BitData,
    T2: BitData,
    [(); (T1::BITS + T2::BITS) as usize / 8]:,
{
    const BITS: u8 = T1::BITS + T2::BITS;

    fn encode(&self) -> Result<Vec<u8>, Error> {
        let mut buf: [u8; (T1::BITS + T2::BITS) as usize / 8] =
            [0; (T1::BITS + T2::BITS) as usize / 8];
        let offset = to_buf(&mut buf, self.0.get(), 0, (T1::BITS) as usize);
        to_buf(&mut buf, self.1.get(), offset, (T2::BITS) as usize);

        Ok(buf.into_iter().collect::<Vec<_>>())
    }

    fn decode(data: &[u8]) -> Result<Self, Error>
    where
        Self: Sized,
    {
        Ok((
            T1::from_u8(from_buf(data, 0, (T1::BITS) as usize))?,
            T2::from_u8(from_buf(data, (T1::BITS) as usize, (T2::BITS) as usize))?,
        ))
    }
}

impl<T1, T2, T3> BitEncode for (T1, T2, T3)
where
    T1: BitData,
    T2: BitData,
    T3: BitData,
    [(); (T1::BITS + T2::BITS + T3::BITS) as usize / 8]:,
{
    const BITS: u8 = T1::BITS + T2::BITS + T3::BITS;

    fn encode(&self) -> Result<Vec<u8>, Error> {
        let mut buf: [u8; (T1::BITS + T2::BITS + T3::BITS) as usize / 8] =
            [0; (T1::BITS + T2::BITS + T3::BITS) as usize / 8];
        let offset = to_buf(&mut buf, self.0.get(), 0, (T1::BITS) as usize);
        let offset = to_buf(&mut buf, self.1.get(), offset, (T2::BITS) as usize);
        to_buf(&mut buf, self.2.get(), offset, (T3::BITS) as usize);

        Ok(buf.into_iter().collect::<Vec<_>>())
    }

    fn decode(data: &[u8]) -> Result<Self, Error>
    where
        Self: Sized,
    {
        Ok((
            T1::from_u8(from_buf(data, 0, (T1::BITS) as usize))?,
            T2::from_u8(from_buf(data, (T1::BITS) as usize, (T2::BITS) as usize))?,
            T3::from_u8(from_buf(
                data,
                (T1::BITS + T2::BITS) as usize,
                (T3::BITS) as usize,
            ))?,
        ))
    }
}

impl<T1, T2, T3, T4> BitEncode for (T1, T2, T3, T4)
where
    T1: BitData,
    T2: BitData,
    T3: BitData,
    T4: BitData,
    [(); (T1::BITS + T2::BITS + T3::BITS + T4::BITS) as usize / 8]:,
{
    const BITS: u8 = T1::BITS + T2::BITS + T3::BITS + T4::BITS;

    fn encode(&self) -> Result<Vec<u8>, Error> {
        let mut buf: [u8; (T1::BITS + T2::BITS + T3::BITS + T4::BITS) as usize / 8] =
            [0; (T1::BITS + T2::BITS + T3::BITS + T4::BITS) as usize / 8];
        let offset = to_buf(&mut buf, self.0.get(), 0, (T1::BITS) as usize);
        let offset = to_buf(&mut buf, self.1.get(), offset, (T2::BITS) as usize);
        let offset = to_buf(&mut buf, self.2.get(), offset, (T3::BITS) as usize);
        to_buf(&mut buf, self.3.get(), offset, (T4::BITS) as usize);

        Ok(buf.into_iter().collect::<Vec<_>>())
    }

    fn decode(data: &[u8]) -> Result<Self, Error>
    where
        Self: Sized,
    {
        Ok((
            T1::from_u8(from_buf(data, 0, (T1::BITS) as usize))?,
            T2::from_u8(from_buf(data, (T1::BITS) as usize, (T2::BITS) as usize))?,
            T3::from_u8(from_buf(
                data,
                (T1::BITS + T2::BITS) as usize,
                (T3::BITS) as usize,
            ))?,
            T4::from_u8(from_buf(
                data,
                (T1::BITS + T2::BITS + T3::BITS) as usize,
                (T4::BITS) as usize,
            ))?,
        ))
    }
}

impl<T1, T2, T3, T4, T5> BitEncode for (T1, T2, T3, T4, T5)
where
    T1: BitData,
    T2: BitData,
    T3: BitData,
    T4: BitData,
    T5: BitData,
    [(); (T1::BITS + T2::BITS + T3::BITS + T4::BITS + T5::BITS) as usize / 8]:,
{
    const BITS: u8 = T1::BITS + T2::BITS + T3::BITS + T4::BITS + T5::BITS;

    fn encode(&self) -> Result<Vec<u8>, Error> {
        let mut buf: [u8; (T1::BITS + T2::BITS + T3::BITS + T4::BITS + T5::BITS) as usize / 8] =
            [0; (T1::BITS + T2::BITS + T3::BITS + T4::BITS + T5::BITS) as usize / 8];
        let offset = to_buf(&mut buf, self.0.get(), 0, (T1::BITS) as usize);
        let offset = to_buf(&mut buf, self.1.get(), offset, (T2::BITS) as usize);
        let offset = to_buf(&mut buf, self.2.get(), offset, (T3::BITS) as usize);
        let offset = to_buf(&mut buf, self.3.get(), offset, (T4::BITS) as usize);
        to_buf(&mut buf, self.4.get(), offset, (T5::BITS) as usize);

        Ok(buf.into_iter().collect::<Vec<_>>())
    }

    fn decode(data: &[u8]) -> Result<Self, Error>
    where
        Self: Sized,
    {
        Ok((
            T1::from_u8(from_buf(data, 0, (T1::BITS) as usize))?,
            T2::from_u8(from_buf(data, (T1::BITS) as usize, (T2::BITS) as usize))?,
            T3::from_u8(from_buf(
                data,
                (T1::BITS + T2::BITS) as usize,
                (T3::BITS) as usize,
            ))?,
            T4::from_u8(from_buf(
                data,
                (T1::BITS + T2::BITS + T3::BITS) as usize,
                (T4::BITS) as usize,
            ))?,
            T5::from_u8(from_buf(
                data,
                (T1::BITS + T2::BITS + T3::BITS + T4::BITS) as usize,
                (T5::BITS) as usize,
            ))?,
        ))
    }
}
