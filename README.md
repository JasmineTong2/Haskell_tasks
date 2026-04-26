# CNSCC212 Universal Turing Machine
# 26/04/2026
This project implements the coursework Universal Turing Machine in Haskell.

## Build

```powershell
cabal build
```

## Run M1

Mode `M1` prints exactly one terminal line: `ACCEPTED` or `REJECTED`.

```powershell
cabal run coursework -- C:\absolute\path\to\machine.desc 0011001 M1
```

## Run M2

Mode `M2` opens a pure Haskell Win32 GUI window showing the tape, head, state, verdict, and the rule table.

```powershell
cabal run coursework -- C:\absolute\path\to\machine.desc 0011001 M2
```

## Description Format

The `.desc` files follow the coursework format:

```text
initialState=q0
acceptState=qa
rejectState=qr
variant=CLASSICAL
rules=q0,0,qa,0,RIGHT<>q0,1,qr,1,RIGHT
```

The implementation supports `CLASSICAL`, `LRTM`, and `BBTM`.

--by Tong. You know, for haskell--