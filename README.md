# Sojobo - A binary analysis framework

_Sojobo_ is an emulator for the <a href="https://b2r2.org/" target="_blank">B2R2</a> framework. It was created to easier the analysis of potentially malicious files. It is totally developed in .NET so you don't need to install or compile any other external libraries (the project is self contained).

With _Sojobo_ you can:
* Emulate a (32 bit) PE binary
* Inspect the memory of the emulated process
* Read the process state
* Display a disassembly of the executed code
* Emulate functions in a managed language (C# || F#)

## Tools using Sojobo
- ADVDeobfuscator

# ADV Deobfuscator - A string deobfuscator for ADVObfuscator

_ADVDeobfuscator_ is tool based on the Sojobo binary analysis framework that analyzes a binary obfuscated with ADBObfuscator and decodes the identified strings.

## Download

A compiled version is available to <a href="https://github.com/sponsors/enkomio">Community sponsored users</a>. If you are a sponsored user you can download the binary from: <a href="https://github.com/enkomio-sponsor/compiled_binaries">https://github.com/enkomio-sponsor/compiled_binaries</a>

## Documentation
The image below shows an execution of ADVDeobfuscator on the <a href="https://www.carbonblack.com/blog/tau-threat-discovery-conti-ransomware/">Conti Ransomware</a>.

<img src="https://github.com/enkomio/Sojobo/blob/master/Images/ADVDeobfuscator_Conti.gif">

The image below shows an execution of ADVDeobfuscator on the <a href="https://www.zscaler.com/blogs/security-research/taurus-new-stealer-town/">Taurus Stealer</a> (see also <a href="https://fumik0.com/2019/12/25/lets-play-again-with-predator-the-thief/">Predator the thief</a>).

<img src="https://github.com/enkomio/Sojobo/blob/master/Images/ADVDeobfuscator_taurus.gif">

I wrote a <a href="http://antonioparata.blogspot.com/2020/06/deobfuscating-c-advobfuscator-with.html">blog post on how to deobfuscate the Team 9 binaries</a>.

# Using Sojobo

_Sojobo_ is intended to be used as a framework to create program analysis utilities. However, various <a href="https://github.com/enkomio/Sojobo/tree/master/Src/Examples"><strong>sample utilities</strong></a> were created in order to show how to use the framework in a profitable way. 

## Download

 - [Source code][1]
 - [Download binary][2]

## Documentation
The project is fully documented in F# (cit.) :) Joking apart, I plan to write some blog posts related to how to use Sojobo. Below a list of the current posts:

 - <a href="https://antonioparata.blogspot.com/2019/05/sojobo-yet-another-binary-analysis.html">Sojobo - Yet another binary analysis framework</a>
 
You can also read the <strong><a href="https://github.com/enkomio/Sojobo/blob/master/DOCUMENTATION.md">API documentation</a></strong>.

## Compile

In order to compile Sojobo you need .NET Core to be installed and Visual Studio. To compile just run **build.bat**.

## License

Copyright (C) 2019 Antonio Parata - <a href="https://twitter.com/s4tan">@s4tan</a>

_Sojobo_ is licensed under the [Creative Commons](LICENSE.md).

  [1]: https://github.com/enkomio/sojobo/tree/master/Src
  [2]: https://github.com/enkomio/sojobo/releases/latest
