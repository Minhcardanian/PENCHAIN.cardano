# PenChain Cardano MVP

This repository contains a very small Haskell prototype inspired by the PenChain paper. It demonstrates how an SLA contract might be implemented using Cardano's Plutus libraries.

## Structure

- `src/` - all source files
  - `Main.hs` – executable demonstration
  - `SLA.hs` – simplified on-chain contract
  - `ServiceAggregation.hs` – data type for SLA aggregation
  - `Ranking.hs` – basic SLA ranking
  - `Reputation.hs` – minimal provider reputation model

## Building

A recent GHC with Cardano and Plutus dependencies is required.  The project is configured for both **cabal** and **stack**.

### Cabal

```bash
cabal build
```

### Stack

```bash
stack build
```

These commands will produce the `penchain-cardano` executable which prints a short ranking demo.  You may need to provide the Plutus packages locally for a successful build.

### Dependencies

The demo expects the Cardano **plutus-ledger-api** and related packages to be
available in your package database.  In addition, `containers` and `text` from
the Haskell Platform are required.

