use int_enum::IntEnum;
use mzsh_bitenum::{BitEnum, BitEnumTrait};

pub(crate) const fn _f_bit_guard<const BITS: u8>() -> bool {
    if !(BITS <= 8 && BITS > 0) {
        panic!("guard evaluated to false")
    }
    true
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct UintX<const BITS: u8>(pub u8)
where const_guards::Guard<{
    _f_bit_guard::<BITS>()
}>: const_guards::Protect;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct EnumUIntX<T, const BITS: u8>(pub T)
where T: IntEnum,
const_guards::Guard<{
    _f_bit_guard::<BITS>()
}>: const_guards::Protect;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct BitEnumUIntX<T, const BITS: u8>(pub BitEnum<T>)
where T: Sized + int_enum::IntEnum,
<T as int_enum::IntEnum>::Int: Default,
<T as int_enum::IntEnum>::Int: bincode::Encode,
<T as int_enum::IntEnum>::Int: bincode::Decode,
<T as int_enum::IntEnum>::Int: bincode::BorrowDecode<'static>,
const_guards::Guard<{
    _f_bit_guard::<BITS>()
}>: const_guards::Protect;

pub(crate) trait BitData
where Self: Sized
{
    const BITS: u8;
    fn from_u8(data: u8) -> Option<Self>;
    fn get(&self) -> u8;
}

impl<const BITS: u8> BitData for UintX<BITS>
where const_guards::Guard<{
    _f_bit_guard::<BITS>()
}>: const_guards::Protect
{
    const BITS: u8 = BITS;
    
    fn from_u8(data: u8) -> Option<Self> {
        Some(Self(data))
    }

    fn get(&self) -> u8 {
        self.0
    }
}

impl<T, const BITS: u8> BitData for EnumUIntX<T, BITS>
where T: IntEnum,
<T as IntEnum>::Int: num::NumCast,
<T as IntEnum>::Int: Default,
const_guards::Guard<{
    _f_bit_guard::<BITS>()
}>: const_guards::Protect
{
    const BITS: u8 = BITS;
    
    fn from_u8(data: u8) -> Option<Self> {
        T::from_int(num::cast(data).unwrap_or_default()).ok().map(|x| Self(x))
    }

    fn get(&self) -> u8 {
        num::cast(self.0.int_value()).unwrap_or_default()
    }
}

impl<T, const BITS: u8> BitData for BitEnumUIntX<T, BITS>
where T: Sized + int_enum::IntEnum,
<T as int_enum::IntEnum>::Int: Default,
<T as int_enum::IntEnum>::Int: bincode::Encode,
<T as int_enum::IntEnum>::Int: bincode::Decode,
<T as int_enum::IntEnum>::Int: bincode::BorrowDecode<'static>,
<T as IntEnum>::Int: num::NumCast,
const_guards::Guard<{
    _f_bit_guard::<BITS>()
}>: const_guards::Protect
{
    const BITS: u8 = BITS;

    fn from_u8(data: u8) -> Option<Self> {
        T::from_int(num::cast(data).unwrap_or_default()).ok().map(|x| Self(BitEnum::<T>::from_vec(vec![x])))
    }

    fn get(&self) -> u8 {
        num::cast(self.0.get_val()).unwrap_or_default()
    }
}


pub(crate) trait BitEncode
where Self: Sized
{
    const BITS: u8;

    fn encode(&self) -> Result<Vec<u8>, bincode::error::EncodeError>;
    fn decode(data: &[u8]) -> Result<Self, bincode::error::DecodeError>;
    fn decode_from_reader<R: bincode::de::read::Reader>(reader: &mut R) -> Result<Self, bincode::error::DecodeError>
    where [(); Self::BITS as usize / 8]: ,
    {
        let mut data = [0u8; Self::BITS as usize / 8];
        R::read(reader, &mut data)?;
        Self::decode(&data)
    }
}

fn from_buf(buf: &[u8], start: usize, size: usize) -> u8
{
    let byte_offset = start / 8;
    let bit_offset = start % 8;
    let end = bit_offset + size;
    let mut b1 = buf[byte_offset] & (0xFF >> (bit_offset));
    let mut b2 = 0;
    if end <= 8 {
        b1 = b1 >> (8 - end);
    } else {
        b1 = b1 << (size - (16 - end));
        b2 = buf[byte_offset + 1] >> (16 - end);
    }

    b1 | b2
}

fn to_buf(buf: &mut [u8], data: u8, offset: usize, size: usize) -> usize
{
    let current_offset_end = size + (offset % 8);
    let val = match current_offset_end {
        x if x <= 8 => (data << (8 - x), None), //fits in the same byte
        x if x > 8 => (data >> (x - 8), Some((data & (0xFF >> (x - 8))) << (16 - x))),
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
[(); (T1::BITS) as usize / 8]: ,
{
    const BITS: u8 = T1::BITS;

    fn encode(&self) -> Result<Vec<u8>, bincode::error::EncodeError>
    {
        let mut buf: [u8; (T1::BITS) as usize / 8] = [0; (T1::BITS) as usize / 8];
        to_buf(&mut buf, self.0.get(), 0, (T1::BITS) as usize);

        Ok(buf.into_iter().collect::<Vec<_>>())
    }

    fn decode(data: &[u8]) -> Result<Self, bincode::error::DecodeError> where Self: Sized {
        Ok((
            T1::from_u8(from_buf(data, 0, (T1::BITS) as usize)).ok_or(bincode::error::DecodeError::Other("BitEncode trait failed, failed to convert from_u8"))?, 
        ))
    }
}

impl<T1, T2> BitEncode for (T1, T2,)
where
T1: BitData,
T2: BitData,
[(); (T1::BITS + T2::BITS) as usize / 8]: ,
{
    const BITS: u8 = T1::BITS + T2::BITS;

    fn encode(&self) -> Result<Vec<u8>, bincode::error::EncodeError> {
        let mut buf: [u8; (T1::BITS + T2::BITS) as usize / 8] = [0; (T1::BITS + T2::BITS) as usize / 8];
        let offset = to_buf(&mut buf, self.0.get(), 0, (T1::BITS) as usize);
        to_buf(&mut buf, self.1.get(), offset, (T2::BITS) as usize);

        Ok(buf.into_iter().collect::<Vec<_>>())
    }

    fn decode(data: &[u8]) -> Result<Self, bincode::error::DecodeError> where Self: Sized {
        Ok((
            T1::from_u8(from_buf(data, 0, (T1::BITS) as usize)).ok_or(bincode::error::DecodeError::Other("BitEncode trait failed, failed to convert from_u8"))?, 
            T2::from_u8(from_buf(data, (T1::BITS) as usize, (T2::BITS) as usize)).ok_or(bincode::error::DecodeError::Other("BitEncode trait failed, failed to convert from_u8"))?, 
        ))
    }
}

impl<T1, T2, T3> BitEncode for (T1, T2, T3,)
where
T1: BitData,
T2: BitData,
T3: BitData,
[(); (T1::BITS + T2::BITS + T3::BITS) as usize / 8]: ,
{
    const BITS: u8 = T1::BITS + T2::BITS + T3::BITS;

    fn encode(&self) -> Result<Vec<u8>, bincode::error::EncodeError> {
        let mut buf: [u8; (T1::BITS + T2::BITS + T3::BITS) as usize / 8] = [0; (T1::BITS + T2::BITS + T3::BITS) as usize / 8];
        let offset = to_buf(&mut buf, self.0.get(), 0, (T1::BITS) as usize);
        let offset = to_buf(&mut buf, self.1.get(), offset, (T2::BITS) as usize);
        to_buf(&mut buf, self.2.get(), offset, (T3::BITS) as usize);

        Ok(buf.into_iter().collect::<Vec<_>>())
    }

    fn decode(data: &[u8]) -> Result<Self, bincode::error::DecodeError> where Self: Sized {
        Ok((
            T1::from_u8(from_buf(data, 0, (T1::BITS) as usize)).ok_or(bincode::error::DecodeError::Other("BitEncode trait failed, failed to convert from_u8"))?, 
            T2::from_u8(from_buf(data, (T1::BITS) as usize, (T2::BITS) as usize)).ok_or(bincode::error::DecodeError::Other("BitEncode trait failed, failed to convert from_u8"))?, 
            T3::from_u8(from_buf(data, (T1::BITS + T2::BITS) as usize, (T3::BITS) as usize)).ok_or(bincode::error::DecodeError::Other("BitEncode trait failed, failed to convert from_u8"))?, 
        ))
    }
}

impl<T1, T2, T3, T4> BitEncode for (T1, T2, T3, T4,)
where
T1: BitData,
T2: BitData,
T3: BitData,
T4: BitData,
[(); (T1::BITS + T2::BITS + T3::BITS + T4::BITS) as usize / 8]: ,
{
    const BITS: u8 = T1::BITS + T2::BITS + T3::BITS + T4::BITS;

    fn encode(&self) -> Result<Vec<u8>, bincode::error::EncodeError> {
        let mut buf: [u8; (T1::BITS + T2::BITS + T3::BITS + T4::BITS) as usize / 8] = [0; (T1::BITS + T2::BITS + T3::BITS + T4::BITS) as usize / 8];
        let offset = to_buf(&mut buf, self.0.get(), 0, (T1::BITS) as usize);
        let offset = to_buf(&mut buf, self.1.get(), offset, (T2::BITS) as usize);
        let offset = to_buf(&mut buf, self.2.get(), offset, (T3::BITS) as usize);
        to_buf(&mut buf, self.3.get(), offset, (T4::BITS) as usize);

        Ok(buf.into_iter().collect::<Vec<_>>())
    }

    fn decode(data: &[u8]) -> Result<Self, bincode::error::DecodeError> where Self: Sized {
        Ok((
            T1::from_u8(from_buf(data, 0, (T1::BITS) as usize)).ok_or(bincode::error::DecodeError::Other("BitEncode trait failed, failed to convert from_u8"))?, 
            T2::from_u8(from_buf(data, (T1::BITS) as usize, (T2::BITS) as usize)).ok_or(bincode::error::DecodeError::Other("BitEncode trait failed, failed to convert from_u8"))?, 
            T3::from_u8(from_buf(data, (T1::BITS + T2::BITS) as usize, (T3::BITS) as usize)).ok_or(bincode::error::DecodeError::Other("BitEncode trait failed, failed to convert from_u8"))?, 
            T4::from_u8(from_buf(data, (T1::BITS + T2::BITS + T3::BITS) as usize, (T4::BITS) as usize)).ok_or(bincode::error::DecodeError::Other("BitEncode trait failed, failed to convert from_u8"))?, 
        ))
    }
}

impl<T1, T2, T3, T4, T5> BitEncode for (T1, T2, T3, T4, T5,)
where
T1: BitData,
T2: BitData,
T3: BitData,
T4: BitData,
T5: BitData,
[(); (T1::BITS + T2::BITS + T3::BITS + T4::BITS + T5::BITS) as usize / 8]: ,
{
    const BITS: u8 = T1::BITS + T2::BITS + T3::BITS + T4::BITS + T5::BITS;

    fn encode(&self) -> Result<Vec<u8>, bincode::error::EncodeError> {
        let mut buf: [u8; (T1::BITS + T2::BITS + T3::BITS + T4::BITS + T5::BITS) as usize / 8] = [0; (T1::BITS + T2::BITS + T3::BITS + T4::BITS + T5::BITS) as usize / 8];
        let offset = to_buf(&mut buf, self.0.get(), 0, (T1::BITS) as usize);
        let offset = to_buf(&mut buf, self.1.get(), offset, (T2::BITS) as usize);
        let offset = to_buf(&mut buf, self.2.get(), offset, (T3::BITS) as usize);
        let offset = to_buf(&mut buf, self.3.get(), offset, (T4::BITS) as usize);
        to_buf(&mut buf, self.4.get(), offset, (T5::BITS) as usize);

        Ok(buf.into_iter().collect::<Vec<_>>())
    }

    fn decode(data: &[u8]) -> Result<Self, bincode::error::DecodeError> where Self: Sized {
        Ok((
            T1::from_u8(from_buf(data, 0, (T1::BITS) as usize)).ok_or(bincode::error::DecodeError::Other("BitEncode trait failed, failed to convert from_u8"))?, 
            T2::from_u8(from_buf(data, (T1::BITS) as usize, (T2::BITS) as usize)).ok_or(bincode::error::DecodeError::Other("BitEncode trait failed, failed to convert from_u8"))?, 
            T3::from_u8(from_buf(data, (T1::BITS + T2::BITS) as usize, (T3::BITS) as usize)).ok_or(bincode::error::DecodeError::Other("BitEncode trait failed, failed to convert from_u8"))?, 
            T4::from_u8(from_buf(data, (T1::BITS + T2::BITS + T3::BITS) as usize, (T4::BITS) as usize)).ok_or(bincode::error::DecodeError::Other("BitEncode trait failed, failed to convert from_u8"))?, 
            T5::from_u8(from_buf(data, (T1::BITS + T2::BITS + T3::BITS + T4::BITS) as usize, (T5::BITS) as usize)).ok_or(bincode::error::DecodeError::Other("BitEncode trait failed, failed to convert from_u8"))?, 
        ))
    }
}
