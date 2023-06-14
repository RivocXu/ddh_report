# DDH Web App: Report Generation

## Table of Contents
- [Introduction](#introduction)
- [Requirements](#Requirements)
- [Design Overview](#Design-Overview)
  - [System Architecture](#system-architecture)
  - [UML Diagram](#UML-Diagram)
- [Details](#Game-Detail)
- [Reference](#reference)

## Introduction

## Requirements

## Design Overview

### System Architecture
```mermaid
graph LR
  subgraph AWS
    sqs[SQS] -->|Process Message| lambda[Lambda Function]
    lambda -->|Generate Report| ses[SES]
  end

  subgraph Local
    subgraph R/RStudio
      code[R/RStudio Code]
    end
    subgraph Docker
      dockerfile[Dockerfile]
    end
  end

  code -.-> sqs
  lambda -->|Execute| code
  code -.-> ses
  dockerfile --> code

```

## Details

## Reference