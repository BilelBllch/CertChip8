
import CertChip8.Chip8


inductive Instruction where
  | ret                                            -- 00EE : RET

  -- ==========================================
  -- Sauts et Appels
  -- ==========================================
  | jump (nnn : Fin 4096)                          -- 1nnn : JP addr
  | call (nnn : Fin 4096)                          -- 2nnn : CALL addr
  | jumpV0 (nnn : Fin 4096)                        -- Bnnn : JP V0, addr

  -- ==========================================
  -- Branchements Conditionnels (Skip)
  -- ==========================================
  | skipIfEq (x : Fin 16) (nn : UInt8)            -- 3xnn : SE Vx, byte
  | skipIfNeq (x : Fin 16) (nn : UInt8)             -- 4xnn : SNE Vx, byte
  | skipIfEqReg (x y : Fin 16)                       -- 5xy0 : SE Vx, Vy
  | skipIfNeqReg (x y : Fin 16)                        -- 9xy0 : SNE Vx, Vy

  -- ==========================================
  -- Assignation et Opérations Basiques
  -- ==========================================
  | setRegConst (x : Fin 16) (nn : UInt8)            -- 6xnn : LD Vx, byte
  | addRegConst (x : Fin 16) (nn : UInt8)            -- 7xnn : ADD Vx, byte
  | setRegReg (x y : Fin 16)                       -- 8xy0 : LD Vx, Vy

  -- ==========================================
  -- Arithmétique et Logique (Série 8xyN)
  -- ==========================================
  | or (x y : Fin 16)                           -- 8xy1 : OR Vx, Vy
  | and (x y : Fin 16)                          -- 8xy2 : AND Vx, Vy
  | xor (x y : Fin 16)                          -- 8xy3 : XOR Vx, Vy
  | addRegReg (x y : Fin 16)                       -- 8xy4 : ADD Vx, Vy (Modifie VF)
  | subRegReg (x y : Fin 16)                       -- 8xy5 : SUB Vx, Vy (Modifie VF)
  | rshift (x : Fin 16)                          -- 8xy6 : SHR Vx {, Vy} (Modifie VF)
  | subn (x y : Fin 16)                         -- 8xy7 : SUBN Vx, Vy (Modifie VF)
  | lshift (x : Fin 16)                          -- 8xyE : SHL Vx {, Vy} (Modifie VF)

  -- ==========================================
  -- Opérations sur le Registre d'Index (I)
  -- ==========================================
  | setI (nnn : Fin 4096)                          -- Annn : LD I, addr
  | addIReg (x : Fin 16)                           -- Fx1E : ADD I, Vx

  -- ==========================================
  -- Graphisme
  -- ==========================================
  | draw (x y : Fin 16) (n : Fin 16)               -- Dxyn : DRW Vx, Vy, nibble
  | clear                                          -- 00E0 : CLS


  | rand (x : Fin 16) (nn : UInt8)                 -- Cxnn : RND Vx, byte

  -- ==========================================
  -- Entrées Clavier
  -- ==========================================
  | skipIfPressed (x : Fin 16)                      -- Ex9E : SKP Vx
  | skipIfNotPressed (x : Fin 16)                    -- ExA1 : SKNP Vx
  | waitKey (x : Fin 16)                           -- Fx0A : LD Vx, K (Bloquant)

  -- ==========================================
  -- Timers
  -- ==========================================
  | getDelayTimer (x : Fin 16)                     -- Fx07 : LD Vx, DT
  | setDelayTimer (x : Fin 16)                     -- Fx15 : LD DT, Vx
  | setSoundTimer (x : Fin 16)                     -- Fx18 : LD ST, Vx

  -- ==========================================
  -- Mémoire et BCD (Binary-Coded Decimal)
  -- ==========================================
  | setISprite (x : Fin 16)                        -- Fx29 : LD F, Vx
  | setIBCD (x : Fin 16)                           -- Fx33 : LD B, Vx
  | storeRegs (x : Fin 16)                         -- Fx55 : LD [I], Vx
  | loadRegs (x : Fin 16)                          -- Fx65 : LD Vx, [I]



def fetchOpcode (cpu : State) : Option UInt16 :=
  if cpu.pc == 4095 then none
  else
    let byte1 := (cpu.memory cpu.pc).toUInt16
    let byte2 := (cpu.memory (cpu.pc + 1)).toUInt16
    some ((byte1 <<< 8) ||| byte2)

def decodeOpcode (op : UInt16) : Option Instruction :=
  let n1 := (op >>> 12)
  let n2 := (op >>> 8) &&& 0x0F
  let n3 := (op >>> 4) &&& 0x0F
  let n4 := op &&& 0x0F

  let nnn : Fin 4096 := Fin.ofNat 4096 (op &&& 0x0FFF).toNat
  let nn := (op &&& 0x00FF).toUInt8

  let x : Fin 16 := Fin.ofNat 16 n2.toNat
  let y : Fin 16 := Fin.ofNat 16 n3.toNat
  let n : Fin 16 := Fin.ofNat 16 n4.toNat


  match n1, n2, n3, n4 with
    | 0, 0, 0xE, 0    => some  Instruction.clear
    | 0, 0, 0xE, 0xE  => some  Instruction.ret
    | 1, _, _, _ => some (Instruction.jump nnn)
    | 2, _, _, _ => some (Instruction.call nnn)
    | 3, _, _, _ => some (Instruction.skipIfEq x nn)
    | 4, _, _, _ => some (Instruction.skipIfNeq x nn)
    | 5, _, _, 0 => some (Instruction.skipIfEqReg x y)
    | 6, _, _, _ => some (Instruction.setRegConst x nn)
    | 7, _, _, _ => some (Instruction.addRegConst x nn)
    | 8, _, _, 0 => some (Instruction.setRegReg x y)
    | 8, _, _, 1 => some (Instruction.or x y)
    | 8, _, _, 2 => some (Instruction.and x y)
    | 8, _, _, 3 => some (Instruction.xor x y)
    | 8, _, _, 4 => some (Instruction.addRegReg x y)
    | 8, _, _, 5 => some (Instruction.subRegReg x y)
    | 8, _, _, 6 => some (Instruction.rshift x)
    | 8, _, _, 7 => some (Instruction.subn x y)
    | 8, _, _, 0xE => some  (Instruction.lshift x)
    | 9, _, _, 0 => some  (Instruction.skipIfNeqReg x y)
    | 0xA, _, _, _ => some (Instruction.setI nnn)
    | 0xB, _, _, _ => some (Instruction.jumpV0 nnn)
    | 0xC, _, _, _ => some (Instruction.rand x nn)
    | 0xD, _, _, _ => some (Instruction.draw x y n)
    | 0xE, _, 9, 0xE => some (Instruction.skipIfPressed x)
    | 0xE, _, 0xA, 1 => some (Instruction.skipIfNotPressed x)
    | 0xF, _, 0, 7 => some (Instruction.getDelayTimer x)
    | 0xF, _, 0, 0xA => some (Instruction.waitKey x)
    | 0xF, _, 1, 5 => some (Instruction.setDelayTimer x)
    | 0xF, _, 1, 8 => some (Instruction.setSoundTimer x)
    | 0xF, _, 1, 0xE => some (Instruction.addIReg x)
    | 0xF, _, 2, 9 => some (Instruction.setISprite x)
    | 0xF, _, 3, 3 => some (Instruction.setIBCD x)
    | 0xF, _, 5, 5 => some (Instruction.storeRegs x)
    | 0xF, _, 6, 5 => some (Instruction.loadRegs x)
    | _, _, _, _ => none



def ret (cpu: State) : State :=
  let newsp := cpu.sp - 1
  let savedpc := cpu.stack newsp
  let nextpc : Fin 4096 := Fin.ofNat 4096 (savedpc + 2).toNat
  {cpu with sp := newsp, pc := nextpc}

def call (cpu: State) (nnn : Fin 4096): State :=
  let newstack := λi => if i == cpu.sp then cpu.pc else cpu.stack i
  let newsp := cpu.sp + 1
  let newpc := nnn
  {cpu with stack := newstack, sp := newsp, pc := newpc}

def jump (cpu: State) (nnn: Fin 4096): State := {cpu with pc := nnn}

def jumpV0 (cpu: State) (nnn : Fin 4096) : State :=
  let newpc := (Fin.ofNat 4096 (cpu.vs 0).toNat) + nnn
  {cpu with pc := newpc}

def skipIfNeq (cpu: State) (x: Fin 16) (nn: UInt8): State :=
  let newpc := if (cpu.vs x) == nn then (cpu.pc + 2) else (cpu.pc + 4)
  {cpu with pc := newpc}

def skipIfEq (cpu: State) (x: Fin 16) (nn: UInt8): State :=
  let newpc := if (cpu.vs x) != nn then (cpu.pc + 2) else (cpu.pc + 4)
  {cpu with pc := newpc}

def skipIfNeqReg (cpu: State) (x y : Fin 16) : State :=
  let newpc := if (cpu.vs x) == (cpu.vs y) then (cpu.pc + 2) else (cpu.pc + 4)
  {cpu with pc := newpc}

def skipIfEqReg (cpu: State) (x y : Fin 16) : State :=
  let newpc := if (cpu.vs x) != (cpu.vs y) then (cpu.pc + 2) else (cpu.pc + 4)
  {cpu with pc := newpc}

def setRegConst (cpu: State) (x : Fin 16) (nn: UInt8) : State :=
  let newvs := λi => if i == x then nn else cpu.vs i
  {cpu with vs := newvs, pc :=cpu.pc+2}

def addRegConst (cpu: State) (x : Fin 16) (nn: UInt8) : State :=
  let newvs := λi => if i == x then ((cpu.vs i) + nn) else cpu.vs i
  {cpu with vs := newvs, pc := cpu.pc+2}

def setRegReg (cpu: State) (x y :Fin 16) : State :=
  let newvs := λi => if i == x then (cpu.vs y) else cpu.vs i
  {cpu with vs := newvs, pc := cpu.pc+2}

def or (cpu: State) (x y : Fin 16) : State :=
  let newvs :=  λi => if i == x then ((cpu.vs x) ||| (cpu.vs y)) else cpu.vs i
  {cpu with vs := newvs, pc := cpu.pc+2}

def and (cpu: State) (x y : Fin 16) : State :=
  let newvs :=  λi => if i == x then ((cpu.vs x) &&& (cpu.vs y)) else cpu.vs i
  {cpu with vs := newvs, pc := cpu.pc+2}

def xor (cpu: State) (x y : Fin 16) : State :=
  let newvs :=  λi => if i == x then ((cpu.vs x) ^^^ (cpu.vs y)) else cpu.vs i
  {cpu with vs := newvs, pc := cpu.pc+2}

def addRegReg (cpu: State) (x y : Fin 16) : State :=
  let add := cpu.vs x + cpu.vs y
  let newvf := if add > 255 then 1 else 0
  let newvx := add &&& 0xFF
  let newvs :=  λi => if i == x then newvx
                      else
                        if i == 0xf then newvf
                        else cpu.vs i
  {cpu with vs := newvs, pc := cpu.pc+2}

def subRegReg (cpu: State) (x y : Fin 16) : State :=
  let newvf := if cpu.vs x > cpu.vs y then 1 else 0
  let sub := cpu.vs x - cpu.vs y
  --let newvx := sub &&& 0xFF
  let newvs :=  λi => if i == x then sub
                      else
                        if i == 0xf then newvf
                        else cpu.vs i
  {cpu with vs := newvs, pc := cpu.pc+2}

def rshift (cpu: State) (x : Fin 16) : State :=
  let vx := cpu.vs x
  let lsb := vx &&& 1
  let shift := vx >>> 1
  let newvs := λi =>  if i == 15 then lsb
                      else
                        if i == x then shift
                        else cpu.vs i
  {cpu with vs := newvs, pc := cpu.pc + 2}

def subn (cpu: State) (x y: Fin 16) : State :=
  let newvf := if cpu.vs y > cpu.vs x then 1 else 0
  let newvx := cpu.vs y - cpu.vs x
  let newvs :=  λi => if i == x then newvx
                      else
                        if i == 0xf then newvf
                        else cpu.vs i
  {cpu with vs := newvs, pc := cpu.pc+2}
def lshift (cpu: State) (x : Fin 16) : State :=
  let vx := cpu.vs x
  let lsb := vx &&& 1
  let shift := vx <<< 1
  let newvs := λi => if i == 15 then lsb else if i == x then shift else cpu.vs i
  {cpu with vs := newvs, pc := cpu.pc + 2}


def setI (cpu: State) (nnn: Fin 4096) : State := {cpu with I := nnn, pc := cpu.pc+2}

def addIReg (cpu: State) (x:Fin 16) :State :=
  let newI := (Fin.ofNat 4096 (cpu.vs x).toNat) + cpu.I
  {cpu with I := newI, pc:=cpu.pc+2}

def rand (cpu: State) (x: Fin 16) (nn: UInt8) : State :=
  let (randval, next_rng) := randNat cpu.rng 0 255
  let newvx := randval.toUInt8 &&& nn
  let newvs := λi => if i == x then newvx else cpu.vs i
  {cpu with vs := newvs, pc := cpu.pc + 2, rng := next_rng}

def skipIfPressed (cpu: State) (x: Fin 16) : State :=
  let keyx := Fin.ofNat 16 (cpu.vs x).toNat
  let kb_state := cpu.keys keyx
  let newpc := if kb_state then cpu.pc+4 else cpu.pc+2
  {cpu with pc:=newpc}

def skipIfNotPressed (cpu: State) (x: Fin 16) : State :=
  let keyx := Fin.ofNat 16 (cpu.vs x).toNat
  let kb_state := cpu.keys keyx
  let newpc := if kb_state then cpu.pc+2 else cpu.pc+4
  {cpu with pc:=newpc}


def waitKey (cpu: State) (x: Fin 16) : State :=
  let keys_state := List.range 16 |>.find? (λi =>cpu.keys (Fin.ofNat 16 i))
  match keys_state with
  | some key => let newvs := λi => if i == x then key.toUInt8 else cpu.vs i
                {cpu with pc:= cpu.pc+2, vs:=newvs}
  | none => cpu

def getDt (cpu: State) (x: Fin 16) : State :=
  let newvs := λi => if i == x then cpu.delayTimer else cpu.vs i
  {cpu with pc:= cpu.pc+2, vs:=newvs}

def setDt (cpu: State) (x: Fin 16) : State := {cpu with delayTimer:=cpu.vs x}

def setSoundTimer (cpu: State) (x: Fin 16) : State := {cpu with soundTimer := cpu.vs x}

def setISprite (cpu: State) (x: Fin 16) : State :=
  let vx := (cpu.vs x).toNat
  let sprite_addr := vx*5
  let newI := Fin.ofNat 4096 sprite_addr
  {cpu with I:=newI, pc:=cpu.pc+2}

def setIBCD (cpu: State) (x: Fin 16) : State :=
  let vx := (cpu.vs x).toNat
  let hundreds := (vx / 100).toUInt8
  let tens := ((vx % 100) / 10).toUInt8
  let ones := (vx % 10).toUInt8

  let new_memory := λaddr =>
    if addr.val == cpu.I.val then hundreds
    else if addr.val == cpu.I.val + 1 then tens
    else if addr.val == cpu.I.val + 2 then ones
    else cpu.memory addr

  {cpu with memory := new_memory, pc := cpu.pc+2}

def storeRegs (cpu: State) (x: Fin 16) : State :=
  let newmemory :=
    λaddr =>
      let offset := (addr - cpu.I).toNat
      if offset ≤ x then cpu.vs (Fin.ofNat 16 offset)
      else cpu.memory addr
  {cpu with memory:=newmemory, pc:=cpu.pc+2}

def loadRegs (cpu: State) (x: Fin 16) : State :=
  let newvs :=
    λreg =>
      if reg ≤ x then cpu.memory (Fin.ofNat 4096 (cpu.I.toNat + reg.toNat))
      else cpu.vs reg
  {cpu with vs:=newvs}

def clear (cpu: State) : State := {cpu with screen:= λ_ => λ_ => False, pc := cpu.pc+2}

def draw (cpu: State) (x y n: Fin 16) : State :=

      -- 1. Extraction des coordonnées de départ et application du modulo matériel
      let start_x := (cpu.vs x).toNat % 64
      let start_y := (cpu.vs y).toNat % 32
      let n_lines := n.val

      -- 2. État initial de l'accumulateur de notre réduction : (Écran actuel, VF à 0)
      let init_acc : ((Fin 64 → Fin 32 → Bool) × UInt8) := (cpu.screen, 0)

      -- 3. Boucle externe : itération sur les N lignes du sprite [0, n_lines - 1]
      let (final_screen, vf_flag) := List.range n_lines |>.foldl (fun acc_row row =>
        if start_y + row >= 32 then
          acc_row -- Clipping vertical : le sprite est coupé si on dépasse le bas de l'écran
        else
          let mem_y := Fin.ofNat 32 (start_y + row)
          -- Lecture de l'octet correspondant à la ligne courante du sprite en mémoire
          let sprite_byte := cpu.memory (Fin.ofNat 4096 (cpu.I.val + row))

          -- Boucle interne : itération sur les 8 bits (colonnes) de l'octet [0, 7]
          List.range 8 |>.foldl (fun acc_col col =>
            if start_x + col >= 64 then
              acc_col -- Clipping horizontal : le sprite est coupé si on dépasse la droite
            else
              let mem_x := Fin.ofNat 64 (start_x + col)
              -- Extraction du bit courant du sprite (du bit de poids fort au bit de poids faible)
              let sprite_pixel := (sprite_byte >>> (7 - col).toUInt8) &&& 1

              if sprite_pixel == 1 then
                let (curr_screen, curr_coll) := acc_col
                -- Lecture de l'état actuel du pixel sur l'écran
                let pixel_was_on := curr_screen mem_x mem_y

                -- RÈGLE XOR & COLLISION :
                -- Si le pixel écran était à true, il passe à false (effacement) -> Collision !
                let new_coll := if pixel_was_on then 1 else curr_coll
                let new_screen := fun cx cy =>
                  if cx == mem_x && cy == mem_y then !pixel_was_on else curr_screen cx cy

                (new_screen, new_coll)
              else
                acc_col -- Le bit du sprite est 0, l'écran reste inchangé pour ce pixel

          ) acc_row
      ) init_acc

      -- 4. Mise à jour finale de l'état du CPU
      let new_v := fun i => if i == 15 then vf_flag else cpu.vs i
      let next_pc := Fin.ofNat 4096 (cpu.pc.val + 2)

      { cpu with
          screen := final_screen,
          vs := new_v,
          pc := next_pc
      }
