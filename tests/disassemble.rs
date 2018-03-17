use std::process::Command;

#[test]
fn space_invaders_head() {
    let expected = r#"
   0000 00       NOP
   0001 00       NOP
   0002 00       NOP
   0003 c3 d4 18 JMP    $18d4
   0006 00       NOP
   0007 00       NOP
   0008 f5       PUSH   PSW
   0009 c5       PUSH   BC
   000a d5       PUSH   DE
   000b e5       PUSH   HL
   000c c3 8c 00 JMP    $008c
   000f 00       NOP
   0010 f5       PUSH   PSW
   0011 c5       PUSH   BC
   0012 d5       PUSH   DE
   0013 e5       PUSH   HL
   0014 3e 80    MVI    A,#0x80
   0016 32 72 20 STA    $2072
   "#.trim();

    let out = Command::new("cargo")
        .args(["run", "--bin", "disassemble"].iter())
        .args(["--", "resources/invaders.h"].iter())
        .output().unwrap();

    let stdout = std::str::from_utf8(&out.stdout).unwrap().trim();

    assert!(stdout.contains(expected), "\n{} \n NOT CONTAIN \n {}",
            unsafe {stdout.slice_unchecked(0, (expected.len() + 50))}, expected )
}
