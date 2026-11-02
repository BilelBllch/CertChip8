import CertChip8.Instruction

def execute (cpu : State) (instr : Instruction) : State :=
  match instr with
  | Instruction.ret => ret cpu
  | Instruction.jump nnn => jump cpu nnn
  | Instruction.call nnn => call cpu nnn
  | Instruction.jumpV0 nnn => jumpV0 cpu nnn
  | Instruction.skipIfNeq x nn => skipIfNeq cpu x nn
  | Instruction.skipIfEq x nn =>  skipIfEq cpu x nn
  | Instruction.skipIfNeqReg x y => skipIfNeqReg cpu x y
  | Instruction.skipIfEqReg x y => skipIfEqReg cpu x y
  | Instruction.setRegConst x nn => setRegConst cpu x nn
  | Instruction.addRegConst x nn => addRegConst cpu x nn
  | Instruction.setRegReg x y =>  setRegReg cpu x y
  | Instruction.or x y => or cpu x y
  | Instruction.and x y => and cpu x y
  | Instruction.xor x y => xor cpu x y
  | Instruction.addRegReg x y => addRegReg cpu x y
  | Instruction.subRegReg x y =>  subRegReg cpu x y
  | Instruction.rshift x => rshift cpu x
  | Instruction.subn x y => subn cpu x y
  | Instruction.lshift x => lshift cpu x
  | Instruction.setI nnn => setI cpu nnn
  | Instruction.addIReg x =>  addIReg cpu x
  | Instruction.rand x nn =>  rand cpu x nn
  | Instruction.skipIfPressed x =>  skipIfPressed cpu x
  | Instruction.skipIfNotPressed x => skipIfNotPressed cpu x
  | Instruction.waitKey x =>  waitKey cpu x
  | Instruction.getDelayTimer x =>  getDt cpu x
  | Instruction.setDelayTimer x =>  setDt cpu x
  | Instruction.setSoundTimer x =>  setSoundTimer cpu x
  | Instruction.setISprite x => setISprite cpu x
  | Instruction.setIBCD x => setIBCD cpu x
  | Instruction.storeRegs x =>  storeRegs cpu x
  | Instruction.loadRegs x => loadRegs cpu x
  | Instruction.clear => clear cpu
  | Instruction.draw x y n => draw cpu x y n

def step (cpu: State) : State :=
  match fetchOpcode cpu with
  | none => cpu
  | some opcode => match decodeOpcode opcode with
                  | some instr => execute cpu instr
                  | none => {cpu with pc:=cpu.pc+2}
