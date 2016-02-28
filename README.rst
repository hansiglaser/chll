Design Methodology for Custom Reconfigurable Logic Architectures
================================================================

Build your own custom FPGA and embed it in your ASIC or SoC. The reconfigurable
logic architecture is semi-automatically generated and optimized to your
application domain. You can also use this approach with commercial FPGAs, e.g.,
instead of partial reconfiguration.

If your target designs are similar and you do not need the tremendous
flexibility of commercial (embedded) FPGAs, this project helps to reduce the
power consumption, chip area, delay, and configuration data. Therefore the
optimized reconfigurable logic architecture is not fine-grained as commercial
2D island-style FPGAs. Instead, it employs coarse-grained functional units
tailored to your application domain. 

The the reconfigurable architecture is generated as a soft IP core which
consists of VHDL and Verilog designs. These are included in your ASIC or SoC.
The generated designs adhere to the synthesizable hardware description language
standards. 

This project was developed as a PhD thesis [Gla15] at the TU Wien and verified
by manufacturing an SoC. The scientific design *methodology* was implemented as
an EDA design *flow*. The tools and the design of an exemplary SoC are included
in this project.

Johann.Glaser@gmx.at

.. image:: examples/wsn-soc/doc/Chip-Photos/Chip-Package-Open.jpg?raw=true
   :width: 400 px
   :alt: Chip in Package with Lid removed
   :align: center


Contents
========

Directory Structure
-------------------

::

  README                    this file
  doc/                      documentation, see Sec. Documentation below
  tools/
    flowcmd/                FlowCmd, the command line interface
      templates/            templates for scripts and logic designs
    flowproc/               FlowProc, the central flow tool
    trfsmgen/               TrfsmGen, to generate TR-FSM instances and configuration
    yosys/                  Yosys, a Verilog synthesis tool (external)
    intersynth/             InterSynth, an interconnect generation tool (external)
    yosys-plugin-stubnets/  a Yosys plugin to determine unused nets
    common/                 common source code used by multiple tools
    depends/                experimental Makefile generator
  examples/
    wsn-soc/                the complete WSN SoC RTL design

Warning: The scripts in ``./tools/depends/*`` are highly experimental,
untested, and were not used productively. They implement a bad solution for
dependency tracking. Please improve the scripts or implement a better
dependency handling approach.

External Tools
--------------
Besides the tools provided in this project, the design flow also uses external
tools.

**Yosys Open SYnthesis Suite**
  Yosys is a free framework for Verilog RTL synthesis by Clifford Wolf.

  http://www.clifford.at/yosys/

**InterSynth - Example-Driven Interconnect Synthesis**
  InterSynth is a tool by Clifford Wolf that generates (synthesizes)
  interconnects for heterogeneous coarse-grained reconfigurable logic circuits.

  http://www.clifford.at/intersynth/

**EDA Tools**
  The design flow supports Mentor Graphics Questa Sim and ModelSim for
  simulation and Cadence Conformal LEC for logical equivalence checking.

  For the ASIC or SoC implementation, industry standard tools like Synopsys
  Design Compiler are supported.

OpenMSP430
----------
The WSN SoC includes the OpenMSP430 microcontroller core
http://opencores.org/project,openmsp430 using its SVN revision 191 at
``./examples/wsn-soc/units/openmsp430/``.

Modifications:

- removed ``sync_cell_mclk_wkup`` in ``omsp_clock_module.v``
- adapted ``openMSP430_defines.v`` to the requirements of the WSN SoC
- added ``omsp_clock_gate-dlsg1.v`` with process specific standard cell
- cherry picked a bug fix in the ``omsp_uart.v`` from OpenMSP430 rev. 197
- use Verilog define HIER_MODULE instead of hard coded hierarchical signal
  name in ``msp_debug.v``

Serial Bus Masters
------------------
The WSN SoC includes VHDL code of serial bus masters developed by students at
the TU Wien and supervised by the author of the design flow. The original
designs were slightly modified and placed in
``./examples/wsn-soc/units/*_master/``. Additionally, some testbenches use an
UART core located in ``./examples/wsn-soc/vhdl_packs/units/core/tb/uart/``.

- SPI Master and FIFO by Georg Blemenschitz
- I²C Master by Mario Faschang
- UART by Armin Faltinger

The initial design of the WSN SoC included more bus masters, but these were
removed due to chip area constraints. In some source files there are commented
references.


Documentation
=============

The documentation for this project is stored in ``./doc/``.

- ``abstract.pdf`` and its sources in ``./doc/abstract/`` is a short
  introduction and motivation of this work and the PhD thesis.
- TODO: PhD thesis [Gla15], especially Ch. 3, Ch. 4, and Sec. 5.1
- TODO: journal paper with a condensed introduction to the design methodology
- TODO: tutorial for the design flow
- ``bibliography.bib``: BibTeX file with the scientific publications of the
  author


Build Instructions
==================

Prerequisites
-------------
- The design flow is operated from a Linux shell.
- GNU **Make**
- **FreePascal** compiler, version 2.6.4 was used
- TP Lex and TP Yacc as ``plex`` and ``pyacc``, which are included in the
  FreePascal utilities (e.g. Debian package ``fp-utils-2.6.4``).
  http://www.musikwissenschaft.uni-mainz.de/~ag/tply/
- The tool FlowCmd requires GNU Bash 4.0 or newer.
- The tools FlowProc and TrfsmGen were developed with the Lazarus IDE but this
  is not required to build.
- ``pas-tcl`` in ``./tools/common/pas-tcl/`` and ``pas-readline`` in
  ``./tools/common/pas-readline/``. Both projects are referenced using Git
  Submodules, see below.
- GNU Readline development files
- Tcl development files
- **Ronn** to generate the man pages for FlowProc and TrfsmGen (e.g.,
  https://rubygems.org/gems/ronn/, http://rtomayko.github.io/ronn/, or the
  Debian package ``ruby-ronn``)

FlowCmd
-------
You can use FlowCmd from the location in the repository by executing it with
its complete path.
For a more comfortable usage copy (or link) the script, e.g., to
``/usr/local/bin/``.
In any case you have to edit the script and adjust the paths set as variables
TEMPLATEDIR, YOSYS, FLOWPROC, TRFSMGEN, and INTERSYNTH.

``pas-tcl`` and ``pas-readline``
--------------------------------

The projects ``pas-tcl`` and ``pas-readline`` are referenced using `Git
Submodules <http://git-scm.com/book/en/Git-Tools-Submodules>`_. After cloning
this project, you have to add the submodules too.

::

  git submodule init
  git submodule update


FlowProc and TrfsmGen
---------------------
::

  cd ./tools/flowproc/
  make
  cd ../trfsmgen/
  make

FlowProc and TrfsmGen were developed with the Lazarus IDE, therefore also ``.lpi``
files are provided. However, the tools do not require any Lazarus specifics
(especially no GUI). The Makefiles directly use FreePascal to compile the tools, but
have ``lazbuild`` commented out if you like that better.

Both tools implement an interactive command line interface using GNU Readline
and Tcl. For each command a manual page is provided, however many commands are
not yet documented. The generation of the manual pages is implemented with
``genman.sh`` of the ``pas-tcl`` project.

Yosys
-----
Download or clone Yosys from https://github.com/cliffordwolf/yosys and build as
explained in its README, e.g., in ``./tools/yosys/``.
There is also a Debian package ``yosys``.

The WSN SoC was implemented using commit
``3b52121d328d45a5d4269fd0e8de9af948c0216e``.

InterSynth
----------
Download or checkout InterSynth from http://svn.clifford.at/intersynth/trunk/
and build as explained in its README, e.g., in ``./tools/intersynth/``.

The WSN SoC was implemented using revision 62.


Licences
========

Tools
  The tools FlowCmd, FlowProc, and TrfsmGen are distributed under the terms of
  the GNU GPL 2 or later. You can freely use these tools in the development of
  your (commercial) chip designs. If you improve the tools, you have to provide
  their source code, ideally by contacting the author, e.g., with a pull
  request. This however doesn't affect your chip design.

Stubnets Yosys Plugin
  The Stubnets Yosys Plugin in ./tools/yosys-plugin-stubnets/ was developed by
  Clifford Wolf and improved by the author of the design flow. It is free and
  unencumbered software released into the public domain.

FlowCmd Templates
  The FlowCmd templates for scripts and logic designs are distributed under the
  terms of the GNU LGPL 2 or later. You can freely use the design files and
  scripts in your (commercial) chip designs. If you improve the templates, you
  have to provide their source code. This however doesn't affect the other
  parts of your chip design and especially not the actual scripts used in the
  design which were derived from the templates.

Turbo Pascal Lex/Yacc
  FlowProc and TrfsmGen use TP Lex and Yacc from
  http://www.musikwissenschaft.uni-mainz.de/~ag/tply/ which are included in the
  FreePascal utilities package. TP Lex and Yacc are licensed under the terms of
  the GNU GPL 2 or later. Two files ``./tools/flowproc/lexlib.pas`` and
  ``yacclib.pas`` are included in this repository. These were improved to
  support AnsiStrings, much larger parsing buffers, and show the filename,
  line, and column on errors.

Synopsys Liberty Parser
  FlowProc uses token.l and liberty_parser.y (translated to Pascal) of Synopsys
  "liberty_parse v 2.0". These are distributed under the terms of the Synopsys
  Open Source License Version 1.0.

Yosys
  Yosys is an external tool: http://www.clifford.at/yosys/
  Yosys is free software licensed under the ISC license (a GPL compatible
  license that is similar in terms to the MIT license or the 2-clause BSD
  license).

InterSynth
  InterSynth is an external tool: http://www.clifford.at/intersynth/
  InterSynth is free software licensed under the GNU GPL 2 or later.

WSN SoC
  The WSN SoC is distributed under the terms of the GNU LGPL 2 or later. You
  can freely use the design files and scripts in your (commercial) chip
  designs. If you improve the design files or the scripts, you have to provide
  their source code. This however doesn't affect the other parts of your chip
  design.

Serial Bus Masters included in the WSN SoC
  The serial bus masters for SPI and I2C protocols, the UART core as well as
  the commonly used FIFO are copyrighted by the respective authors.

OpenMSP430 included in the WSN SoC
  The OpenMSP430 is licensed under the BSD license.


WSN SoC
=======

The design methodology for custom reconfigurable logic architectures and its
implementation as an EDA design flow were demonstrated with the WSN SoC. This
chip includes a reconfigurable module which implements an autonomous sensor
interface. The chip was manufactured in a 350nm CMOS 4 metal layer process at
AMS. For more details please see Ch. 4 and 5 of [Gla15] and the
(forthcoming) tutorial.

.. image:: examples/wsn-soc/doc/Encounter/encounter-signoff.png?raw=true
   :width: 400 px
   :alt: FPGA Test Setup
   :align: center

This Github repository provides the complete RTL source code of the WSN SoC.
This includes the manually developed example applications
(``./examples/wsn-soc/apps/``) and cells (``./examples/wsn-soc/celllib/``) of
the reconfigurable module as well as all automatically generated files,
especially the reconfigurable module in
``./examples/wsn-soc/units/reconfmodule/chll/out/reconflogic.vhd`` and
``presilicon.v``.

To test the WSN SoC design, it was implemented on a Xilinx Zynq FPGA (not using
its ARM CPUs) on the ZedBoard and connected to external sensors
(``./examples/wsn-soc/units/core/zedboard/``).

.. image:: examples/wsn-soc/doc/Eval-FPGA/Eval-FPGA.jpg?raw=true
   :width: 400 px
   :alt: FPGA Test Setup
   :align: center

The WSN SoC design was synthesized with Synopsys Design Compiler to the AMS C35
standard cell library in the HIT-KIT 3.80 design kit. It also included two
instances of a 128x8 SRAM and four instances of a 2kx8 SRAM for the OpenMSP430.

The layout was generated with Cadence Encounter. This was also used for
parasitics extraction.

Static timing analysis was performed with Synopsys PrimeTime.

Synopsys Formality was used for logical equivalence checking to compare the
netlists with the top level RTL design.

The gate level netlists with timing annotations were simulated with Mentor
QuestaSim.

The gate level layout and netlist were imported to Cadence Virtuoso ICFB, which
supplemented the detailed layout of all standard cells and SRAMs.
Additionally the layout of a project of a colleague, logos, and process
specific cells were added.

Special scripts for Mentor Calibre were used to generate metal fill structures.

The signoff verification (DRC, LVS) was also carried out with Mentor Calibre.

Screenshots of the WSN SoC design as well as photos of the chip are provided in
``./examples/wsn-soc/doc/``. The setup files and scripts for the tools mentioned
above (synthesis and so on) are not included in the repository due to licensing
conditions and non disclosure agreements.

.. image:: examples/wsn-soc/doc/Chip-Photos/Chip-Microscope-2.jpg?raw=true
   :width: 400 px
   :alt: Chip Die Photograph
   :align: center


TR-FSM
======

The **Transition Based Reconfigurable FSM (TR-FSM)** is a reconfigurable
architecture for FSMs. It is utilized in the generated reconfigurable
architectures.

The TR-FSM design is available as VHDL design at
https://github.com/hansiglaser/trfsm. A copy is also included in this
project at ``./examples/wsn-soc/celllib/trfsm/``. The tool TrfsmGen at
``./tools/trfsmgen/`` generates customized instantiations of the TR-FSM and the
configuration data.


TODO
====
- install the design flow tools at central system directories, this also
  requires the automatic adaption of links in ``./tools/flowcmd/flow``.
- document all functions of FlowProc and TrfsmGen with individual man pages
- several VHDL modules (e.g., the serial bus masters) have Scan* ports which
  are unused and should be removed


References
==========
[Gla15]
  Johann Glaser. *Design Methodology for Custom Reconfigurable Logic
  Architectures.* PhD thesis, TU Wien, 2015.
  http://katalog.ub.tuwien.ac.at/AC12648292
