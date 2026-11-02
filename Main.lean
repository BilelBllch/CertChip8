import Raylean
import Chip8

-- On ouvre le namespace pour ne pas avoir à écrire "Raylib." partout
open Raylean
open Raylean.Types
open Raylean.Graphics2D

def scale : Nat := 10
def screenWidth : Nat := 640
def screenHeight : Nat := 320

partial def mainLoop (cpu : State) : IO Unit := do
  -- 1. Appel de la fonction avec une minuscule
  let close ← windowShouldClose
  if close then return ()

  let cpu_ticked := State.tick cpu
  let cpu_next := step cpu_ticked

  beginDrawing

  -- Raylean utilise généralement "Color" plutôt que "Colors"
  clearBackground Color.black

  for y in [0:32] do
    for x in [0:64] do
    -- 2. On dit à Lean de vérifier la borne et on garde la preuve mathématique (hx et hy)
      if hx : x < 64 then
        if hy : y < 32 then
          -- On fournit l'entier ET sa preuve avec la notation ⟨valeur, preuve⟩
          let r : Rectangle := {x := (x*scale).toFloat, y := (y*scale).toFloat, width := scale.toFloat, height := scale.toFloat}
          if cpu_next.screen ⟨x, hx⟩ ⟨y, hy⟩ == true then drawRectangleRec r Color.white

  endDrawing

  mainLoop cpu_next

def main : IO Unit := do
  IO.println "Démarrage de l'émulateur..."
  initWindow screenWidth screenHeight "CertChip8 - 100% Lean 4"
  setTargetFPS 60
  IO.println "Fenêtre initialisée."
  let cpu := State.init 42

  mainLoop cpu

  closeWindow
