use uintx::uintx::BitEncode;
use uintx::uintx::UintX;

#[test]
fn test_most() -> Result<(), String> {
    let ui1 = UintX::<3>(5);
    let ui2 = UintX::<4>(3);
    let ui3 = UintX::<5>(12);
    let ui4 = UintX::<4>(6);

    let us = (ui1, ui2, ui3, ui4).encode(uintx::uintx::BitConfig::BitMostByteSeq);
    let usv = match us {
        Ok(x) => x,
        Err(e) => Err(e.to_string())?,
    };
    assert_eq!(usv, [166u8, 198u8]);
    
    //101 0011 01100 0110
    //10100110 11000110
    let usd: Result<(UintX<3>, UintX<4>, UintX<5>, UintX<4>), _> =
        BitEncode::decode(usv.as_slice(), uintx::uintx::BitConfig::BitMostByteSeq);
    assert_eq!(
        usd.map_err(|e| e.to_string())?,
        (UintX::<3>(5), UintX::<4>(3), UintX::<5>(12), UintX::<4>(6))
    );
    Ok(())
}

#[test]
fn test_least() -> Result<(), String> {
    let ui1 = UintX::<3>(5);
    let ui2 = UintX::<4>(3);
    let ui3 = UintX::<5>(12);
    let ui4 = UintX::<4>(6);

    let us = (ui1, ui2, ui3, ui4).encode(uintx::uintx::BitConfig::BitLeastByteSeq);
    let usv = match us {
        Ok(x) => x,
        Err(e) => Err(e.to_string())?,
    };
    assert_eq!(usv, [101u8, 99u8]);
    
    //0011 101 0110 01100
    //01100101 01100011
    let usd: Result<(UintX<3>, UintX<4>, UintX<5>, UintX<4>), _> =
        BitEncode::decode(usv.as_slice(), uintx::uintx::BitConfig::BitLeastByteSeq);
    assert_eq!(
        usd.map_err(|e| e.to_string())?,
        (UintX::<3>(5), UintX::<4>(3), UintX::<5>(12), UintX::<4>(6))
    );
    Ok(())
}
