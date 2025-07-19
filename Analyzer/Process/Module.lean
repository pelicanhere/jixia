/-
Copyright (c) 2024 BICMR@PKU. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
Authors: Tony Beta Lambda
-/
import Lean
import Analyzer.Types

open Lean Elab Command

namespace Lean

/-- Return the name of the module in which a declaration was defined. -/
def Environment.getModuleFor? (env : Environment) (declName : Name) : Option Name :=
  match env.getModuleIdxFor? declName with
  | none =>
    if env.constants.map₂.contains declName then
      env.header.mainModule
    else
      none
  | some idx => env.header.moduleNames[idx.toNat]!

end Lean

namespace Analyzer.Process.Module

def getResult : CommandElabM ModuleInfo := do
  let env ← getEnv
  let imports := env.header.imports.map fun i => i.module
  let docstring := getMainModuleDoc env |>.toArray |>.map fun d => d.doc
  -- Collect constants and their module names from map₁
  let constants ← env.constants.map₁.foldM (init := #[]) fun acc name _ => do
    if let some moduleName := env.getModuleFor? name then
      return acc.push (name, moduleName)
    else
      return acc
  let constants ← env.constants.map₂.foldlM (init := constants) fun acc name _ => do
    if let some moduleName := env.getModuleFor? name then
      return acc.push (name, moduleName)
    else
      return acc
  return {
    imports,
    docstring,
    constants,
  }

end Analyzer.Process.Module
