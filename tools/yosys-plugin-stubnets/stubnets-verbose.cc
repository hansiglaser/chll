// This is free and unencumbered software released into the public domain.
// 
// Anyone is free to copy, modify, publish, use, compile, sell, or
// distribute this software, either in source code form or as a compiled
// binary, for any purpose, commercial or non-commercial, and by any
// means.
//
// Compile with
//   $ make
// and then either run the built-in tests
//   $ make test
// or run it by hand for your design
//   $ yosys -q -l core.log -m ./stubnets.so ../../examples/wsn-soc/units/core/verilog/{master,core}.v -p "proc; opt; select Core; stubnets -o core-stubnets.txt; write_ilang core.il"

#include "kernel/rtlil.h"
#include "kernel/register.h"
#include "kernel/celltypes.h"
#include "kernel/sigtools.h"
#include "kernel/log.h"

#include <string>
#include <map>
#include <set>
#include <tuple>
#include <stdio.h>

USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN

CellTypes ct;

static const char* WireDirection(const RTLIL::Wire* wire) {
  if (!wire) return "unknown";
  if ( wire->port_input && !wire->port_output) return "in";
  if (!wire->port_input &&  wire->port_output) return "out";
  if ( wire->port_input &&  wire->port_output) return "inout";
  return "unknown";
}
static const char* WireDirection(const RTLIL::IdString celltype, const RTLIL::IdString port) {
  if (!ct.cell_known(celltype)) return "unknown";
  bool input  = ct.cell_input (celltype,port);
  bool output = ct.cell_output(celltype,port);
  if ( input && !output) return "in";
  if (!input &&  output) return "out";
  if ( input &&  output) return "inout";
  return "unknown";
}

/**
 * Find unused nets
 *
 * First all ports of all cells (=sub-modules) are stored to a list. Then all
 * wires of the top-module are examined. If such a wire was not previously seen
 * as connected to a cell port, it is a stub top-module port. If such a wire
 * was only seen by a single cell port, it is a stub net.
 */
static void find_stub_nets(RTLIL::Design *design, RTLIL::Module *module, FILE* outfile) {
  SigMap sigmap(module);    // get all signal mappings of the module
  // associative array to count each reference of a SigChunk
  typedef struct {
    const RTLIL::SigBit*   bit;
    const RTLIL::Cell*     cell;
    const RTLIL::IdString* port;
    int                    port_index;
    int                    wire_index;
  } BitUsageInfo;
  typedef std::vector<BitUsageInfo> BitUsageInfoVec;
  std::map<RTLIL::SigBit, BitUsageInfoVec> bit_usage;

   
  log("Looking for stub wires in module %s:\n",
      RTLIL::id2cstr(module->name));

  for (auto cell : module->cells()) {
    // cell_iter.first is an IdString, cell_iter.second is a Cell*
    log("  Cell: %s of type %s\n",
        RTLIL::id2cstr(cell->name),
        RTLIL::id2cstr(cell->type));
    if (!ct.cell_known(cell->type)) {
      // unknown cell type, can't determine port directions
      log("WARNING: unknown submodule type %s, can't determine port directions\n", RTLIL::id2cstr(cell->type));
    }

    // iterate over all ports (called "connections")
    for (auto &conn : cell->connections()) {
      // conn.first is an IdString, conn.second is a SigSpec
      log("    Port: %s[%d:0] (%s), SigSpec: %s\n",
          RTLIL::id2cstr(conn.first),
          conn.second.size()-1,
          WireDirection(cell->type,conn.first),
          conn.second.as_string().c_str());

      // get signal mapping from cell pin to other side --> "sig" is the other side
      RTLIL::SigSpec sig = sigmap(conn.second);  
      log("      Mapped: [%d:0], SigSpec: %s\n",
          sig.size()-1,
          sig.as_string().c_str());
      int PortIdx = 0;
      // iterate over all individual bits
      for (auto bit : sig.bits()) {
        if (bit.wire != NULL) {
          log("        Bit %s[%d:%d] connected with wire %s[%d:%d][%d:%d]\n",
              RTLIL::id2cstr(conn.first),
              PortIdx+1-1,
              PortIdx,
              RTLIL::id2cstr(bit.wire->name),
              bit.wire->width+bit.wire->start_offset-1,   // property of the whole wire
              bit.wire->start_offset,                     // property of the whole wire
              bit.offset+1-1,                             // selection out of the wire
              bit.offset);
        } else {
          log("        Bit %s[%d:%d] is set to const \"%s\"!\n",
              RTLIL::id2cstr(conn.first),
              PortIdx+1-1,
              PortIdx,
              RTLIL::Const(bit.data).as_string().c_str()); 
        }
        PortIdx = PortIdx + 1;
      }
      log("      Expanded: [%d:0], SigSpec: %s\n",
          sig.size()-1,
          sig.as_string().c_str());
      // iterate over all chunks (here: all individual bits)
      PortIdx = 0;
      for (auto bit : sig.bits()) {
        if (bit.wire != NULL) {
          log("        Bit %s[%d:%d] connected with wire %s[%d:%d][%d:%d]\n",
              RTLIL::id2cstr(conn.first),
              PortIdx+1-1,
              PortIdx,
              RTLIL::id2cstr(bit.wire->name),
              bit.wire->width+bit.wire->start_offset-1,   // property of the whole wire
              bit.wire->start_offset,                     // property of the whole wire
              bit.offset+1-1,                             // selection out of the wire
              bit.offset);
          // store this chunk to be used, i.e. connected to another side
          bit_usage[bit].push_back({&bit, cell, &conn.first, PortIdx, bit.offset});
        } else {
          log("        Bit %s[%d:%d] is set to const \"%s\"!\n",
              RTLIL::id2cstr(conn.first),
              PortIdx+1-1,
              PortIdx,
              RTLIL::Const(bit.data).as_string().c_str()); 
          // const, don't store
        }
        PortIdx = PortIdx + 1;
      }
    }
  }

  log("### Done iterating cells and connections. These chunks were found: ###\n");

  for (auto &c : bit_usage) {
    BitUsageInfoVec bit_usages = c.second;
    int drivers = 0, sinks = 0, inouts = 0;
    log("  Chunk %p %s[%d:%d]: connected to %d port%s:\n",
        &c.first,
        RTLIL::id2cstr(c.first.wire->name),
        1+c.first.offset-1,
        c.first.offset,
        (int)bit_usages.size(),
        bit_usages.size()!=1?"s":"");
    for (auto &cc : bit_usages) {
      // cc is a BitUsageInfo
      log("    %s.%s[%d] %s\n",
          RTLIL::id2cstr(cc.cell->name),
          RTLIL::id2cstr(*cc.port),
          cc.port_index,
          WireDirection(cc.cell->type,*cc.port));
      bool input  = ct.cell_input (cc.cell->type,*cc.port);
      bool output = ct.cell_output(cc.cell->type,*cc.port);
      if ( input && !output) sinks++;
      if (!input &&  output) drivers++;
      if ( input &&  output) inouts++;
    }
    log("    %d drivers, %d sinks, %d inouts\n", drivers, sinks, inouts);
  }
  // some chunks are used only 1 time, others up to 6 times, 0 is not possible
  // because it is initialized with bit_usages[c].push_back()

  log("### Now iterating over all wires of this module: ###\n");

  for (auto wire : module->wires()) {
    log("    Wire: %s[%d:%d], port_id = %d, %s\n",
        RTLIL::id2cstr(wire->name),
        wire->width+wire->start_offset-1,
        wire->start_offset,
        wire->port_id,
        WireDirection(wire));
    // the port_id is only some sort of an auto_incremented counter from the
    // Verilog frontend, it is never used to index into some array or map or
    // so; it is 0 for normal wires and >0 if it is a port of the module it
    // belongs to

    if (!design->selected(module, wire))
      continue;

    // get other side of that wire
    RTLIL::SigSpec sig = sigmap(wire);

    // collect all bits of that wire which are neither constant nor connected
    // to more than 1 side
    //
    // We want to consolidate the single-bit chunks to the original multi-bit
    // chunks. Therefore we store the first bit usage of a contiguous range in
    // bit_usages_first. For every bit we compare the information of the
    // previous bit (stored to bit_usages_prev) with the current bit_usages_this.
    BitUsageInfoVec *bit_usages_first = NULL;
    BitUsageInfoVec *bit_usages_prev  = NULL;
    BitUsageInfoVec *bit_usages_this  = NULL;
    // Note: If we are looking at a top-module port, bit_usages_this will stay NULL,
    // therefore the BitUsageInfo.{port,wire}_index are not usable below when
    // emitting. Since wire_index is identical to i, we add a simple bit index
    // variable to point to the start of the contiguous range.
    size_t bit_index_first = 0;
    enum BitKind {bkNone,bkConst,bkUnknown,bkFeedthrough,bkUsed,bkUnused};
    const char* BitKindLabels[] = {"bkNone","bkConst","bkUnknown","bkFeedthrough","bkUsed","bkUnused"};
    BitKind bit_kind_prev;
    BitKind bit_kind_this = bkNone;
    bool DoEmit = false;
    // Previously this_bit was a references (instead of a pointer), but then
    // very strange things happend: The first assignment (initialization) was
    // an address assignment (i.e., as if it was a pointer), but all other
    // assignments were done as a full copy. There reference pointed to the
    // chunk[0], and all further chunks were copied into this place. :-)
    // To get back to an ordered behavior, we use pointers now.
    RTLIL::SigBit* this_bit = NULL;
    RTLIL::SigBit* prev_bit = NULL;
    for (int i = 0; i < sig.size(); i++) {
      prev_bit = this_bit;
      this_bit = &(sig[i]);
      log("    Chunk %s[%d]\n",RTLIL::id2cstr(wire->name),(int)i);
      if (this_bit && this_bit->wire)   // wire is NULL for constants!
        log("      this_bit %p %s[%d]\n",this_bit, RTLIL::id2cstr(this_bit->wire->name),(int)this_bit->offset);
      if (prev_bit && prev_bit->wire)
        log("      prev_bit %p %s[%d]\n",prev_bit, RTLIL::id2cstr(prev_bit->wire->name),(int)prev_bit->offset);
      bit_usages_prev = bit_usages_this;
      bit_usages_this = NULL;
      bit_kind_prev = bit_kind_this;
      unsigned int drivers = 0, sinks = 0, inouts = 0;
      if (wire->port_id > 0) {
        // wire is a module port: inputs are drivers, outputs are sinks
        if ( wire->port_input && !wire->port_output) drivers++;
        if (!wire->port_input &&  wire->port_output) sinks++;
        if ( wire->port_input &&  wire->port_output) inouts++;
      }
      // check for constant value chunk
      if (this_bit->wire == NULL) {
        log("      skipping constant value %s\n", RTLIL::Const(this_bit->data).as_string().c_str()); 
        bit_kind_this = bkConst;
        goto check_emit;
      }
      // check if this is a feed-through wire
      if (wire != this_bit->wire) {
        log("Warning: Feedthrough between %s[%d:%d] and %s[%d:%d] found.\n",
          RTLIL::id2cstr(wire->name),
          wire->width+wire->start_offset-1, // TODO: this is not exact, because e.g. wire = [7:0], chunk = [3:3]
          wire->start_offset,
          RTLIL::id2cstr(this_bit->wire->name),
          1+this_bit->offset-1,
          this_bit->offset);
        bit_kind_this = bkFeedthrough;
        goto check_emit;
      }
      // check if this chunk was found above
      if (bit_usage.find(*this_bit) == bit_usage.end()) {
        // this is an unknown chunk, so it is not used by any cell and must be
        // a top-module port, but it might still be a feedthrough
        log("      #### this is an unknown chunk, so it is not used by any cell and must be a top-module port: %p %s[%d:%d]: ?\n",
            this_bit,
            RTLIL::id2cstr(this_bit->wire->name),
            1+this_bit->offset-1,
            this_bit->offset);
        bit_kind_this = bkUnknown;
        goto check_emit;
      }
      // we have a known bit
      log("      bit = %p, bit_usage = %d\n",this_bit,(int)bit_usage.count(*this_bit));
      bit_usages_this = &(bit_usage[*this_bit]);
      //  this is a known bit
      log("      this is a known bit: %p %s[%d:%d]: connected to %d port%s: (bit_usages_this = %p)\n",
          this_bit,
          RTLIL::id2cstr(this_bit->wire->name),
          1+this_bit->offset-1,
          this_bit->offset,
          (int)bit_usages_this->size(),
          bit_usages_this->size()!=1?"s":"", bit_usages_this);
      for (auto &cc : *bit_usages_this) {
        // cc is a BitUsageInfo
        if (cc.wire_index != (int)i) {
          log("Internal Error: i != cc.wire_index (%d != %d)\n",(int)i,(int)cc.wire_index);
        }
        log("        %s.%s[%d] (%s%s) <= %s[%d]\n",
            RTLIL::id2cstr(cc.cell->name),
            RTLIL::id2cstr(*cc.port),
            cc.port_index,
            ct.cell_output(cc.cell->name,*cc.port)?"source":"",
            ct.cell_input (cc.cell->name,*cc.port)?"sink":"",
            RTLIL::id2cstr(this_bit->wire->name),
            cc.wire_index
            /*,
            // use "at()" instead of "[]" because it is available in a "const" version
            RTLIL::id2cstr(cc.cell->connections.at(*cc.port).extract(cc.bit_index,1).chunks[0].wire->name)*/);
        bool input  = ct.cell_input (cc.cell->type,*cc.port);
        bool output = ct.cell_output(cc.cell->type,*cc.port);
        if ( input && !output) sinks++;
        if (!input &&  output) drivers++;
        if ( input &&  output) inouts++;
      }
      log("        --> %d drivers, %d sinks, %d inouts\n", drivers, sinks, inouts);

      if (drivers > 0 && sinks > 0) {
        // one driver and one or more sinks --> used
        goto used;
      } else if (drivers+sinks < bit_usages_this->size() && drivers > 0 && bit_usages_this->size() > 1) {
        // we have unknown and/or inout ports, but as long as there is a driver on the net, we 
        goto used;
      } else {
        // only one driver, or one sink, or inouts or unknown --> unused
        goto unused;
      }

unused:
      // ok, here we are sure to have an unused bit in that wire
      log("        --> unused bit!\n");
      bit_kind_this = bkUnused;
      goto check_emit;

used:
      // ok, here we are sure to have a used bit in that wire --> nothing to do
      log("        --> used bit!\n");
      bit_kind_this = bkUsed;
      goto check_emit;

check_emit:
      DoEmit = false;

      // if the bit kind changed the previous contiguous range has to be
      // emitted (except if this is the very first bit)
      if (bit_kind_this != bit_kind_prev) {
        if (bit_kind_prev == bkNone || bit_kind_prev == bkUsed || bit_kind_prev == bkConst) {
          // very first bit, start range; or the previous few bits were used: start range
          log("        very first bit, start range; or the previous few bits were used: start range, bit_usages_first = bit_usages_this (%p)\n",bit_usages_this);
          bit_usages_first = bit_usages_this;
        } else {
          DoEmit = true;
        }
      }

      // if we are amidst an unused range, check if its usage has changed:
      //
      // Simple case: a multi-bit wire is connected to one equal-width port
      //
      // Special case: e.g. one 8 bit port is connected to two 4 bit ports,
      // e.g. SPI_SPPR_SPR = {SPIMaster.SPPR,SPIMaster.SPR}. 
      //
      // Therefore, check whether the bit we are looking at in this iteration
      // used by the very same ports as the bit we were looking at in the
      // previous iteration?
      //
      // bit_usages_prev is NULL if this is the first bit of a contiguous range
      // (or top-module port for which bit_usages_this == NULL)
      if (bit_kind_this == bkUnused && bit_usages_prev) {
        if (bit_usages_prev->size() == bit_usages_this->size()) {
          for (size_t i = 0; i < bit_usages_this->size(); i++) {
            // compare all individual usages of this bit whether they are identical
            BitUsageInfo &Prev = (*bit_usages_prev)[i];
            BitUsageInfo &This = (*bit_usages_this)[i];
            log("        prev = %s.%s-%s-%d-%d, new = %s.%s-%s-%d-%d\n",
                RTLIL::id2cstr(Prev.cell->name),RTLIL::id2cstr(*Prev.port),
                WireDirection(Prev.cell->type,*Prev.port), Prev.port_index, Prev.wire_index,
                RTLIL::id2cstr(This.cell->name),RTLIL::id2cstr(*This.port),
                WireDirection(This.cell->type,*This.port), This.port_index, This.wire_index);
            if ((Prev.cell == This.cell) &&
                (Prev.port == This.port) &&
                (Prev.port_index+1 == This.port_index) &&
                (Prev.wire_index+1 == This.wire_index)) {
              // identical, don't emit
            } else {
              // different usage --> we have such a special case as mentioned above
              DoEmit = true;
              log("        Found a discontinuity!\n");
            }
          }
        } else {
          // different usage with even a different number of usages
          DoEmit = true;
        }
      }

      // if this is the last bit of that wire --> emit
      if ((i+1) == sig.size()) {
        DoEmit = true;
        // trick: bit_usages_first..bit_usages_prev is emitted, so we have to
        // set _prev to _this to get to the end of the vector
        log("        last bit, emitting, setting bit_usages_prev = bit_usages_this = %p\n",bit_usages_this);
        bit_usages_prev = bit_usages_this;
        bit_kind_prev   = bit_kind_this;
        prev_bit        = this_bit;
      }

      // if we would emit something different than an unknown or unused range, don't emit
      if (DoEmit && bit_kind_prev != bkUnknown && bit_kind_prev != bkUnused) {
        log("        Actually, not emitting, bit_kind_prev = %s\n",BitKindLabels[bit_kind_prev]);
        DoEmit = false;
      }

      if (!DoEmit) {
        continue;
      }

      // now really emit
      // Formats:
      //   wire[h:l] dir                     # top-module port
      //   wire[h:l] dir cell.port[h:l]      # cell port
      // where "dir" is "in", "out", "inout" or "unknown"
      if (bit_usages_prev && bit_usages_prev->size() > 0) {
        // unused cell port, there is exactly one usage of that chunk, so
        // simple use [0] of the vector
        log("      bit_usages_first = %p, bit_usages_prev = %p, prev_bit = %p\n", bit_usages_first, bit_usages_prev, &prev_bit);
        BitUsageInfo &First = (*bit_usages_first)[0];
        BitUsageInfo &Prev  = (*bit_usages_prev )[0];
        fprintf(outfile, "%s[%d:%d]\t%s\t%s.%s[%d:%d]\n",
            RTLIL::id2cstr(prev_bit->wire->name),
            Prev.wire_index, First.wire_index,
            WireDirection(Prev.cell->type,*Prev.port),
            RTLIL::id2cstr(Prev.cell->name), RTLIL::id2cstr(*Prev.port),
            Prev.port_index, First.port_index);
        log("      chunk = %p, prev_bit = %p; this_bit->wire = %p, prev_bit->wire = %p; this_chunk->wire->name = %p, prev_bit->wire->name = %p\n",
            this_bit,&prev_bit,this_bit->wire,prev_bit->wire,&(this_bit->wire->name),&(prev_bit->wire->name));
        log("      this_bit->wire->name = %s\n",
            this_bit->wire->name.c_str());
        log("      prev_this_bit->wire->name = %s\n",
            prev_bit->wire->name.c_str());
        log("      ############ Emit: Stub cell %s %s.%s[%d:%d], connected with wire %s[%d:%d]\n",
            WireDirection(Prev.cell->type,*Prev.port),
            RTLIL::id2cstr(Prev.cell->name),RTLIL::id2cstr(*Prev.port),
            Prev.port_index,First.port_index,
            RTLIL::id2cstr(prev_bit->wire->name),
            Prev.wire_index,First.wire_index);
      } else {
        fprintf(outfile, "%s[%d:%d]\t%s\n",
            RTLIL::id2cstr(this_bit->wire->name),
            (int)i,(int)bit_index_first,
            WireDirection(wire));
        log("      ############ Emit: Stub module %s: %s[%d:%d]\n",
            WireDirection(wire),
            RTLIL::id2cstr(this_bit->wire->name),
            (int)i,(int)bit_index_first);
      }
      // store first bit of the following contiguous range
      log("      loop end: setting bit_usages_first = bit_usages_this (%p)\n",bit_usages_this);
      bit_usages_first = bit_usages_this;
      bit_index_first = i;
    }
  }
}

struct StubnetsPass : public Pass {

  StubnetsPass() : Pass("stubnets", "identify unused ports in a module") { }

  virtual void help() {
    //   |---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|
    log("\n");
    log("    stubnets [-o filename]\n");
    log("\n");
    log("This plugin determines all unconnected ports of a module. This are all inputs\n");
    log("and outputs of that module itself as well as all ports of its sub-module\n");
    log("instances (=cells) which are not connected to any other cell port or top\n");
    log("module port.\n");
    log("\n");
    log("    -o filename\n");
    log("        Write information to filename instead of stdout (not log!).\n");
    log("\n");
    log("Format:\n");
    log("  wire[h:l] dir                     # top-module port\n");
    log("  wire[h:l] dir cell.port[h:l]      # cell port\n");
    log("where \"dir\" is \"in\", \"out\", \"inout\" or \"unknown\"\n");
    log("\n");
  }

  virtual void execute(std::vector<std::string> args, RTLIL::Design *design) {
    FILE* outfile = stdout;
    // we could also use yosys/kernel/log.h: std::vector<FILE*> log_files
    // instead of stdout, but this might be a vector with several log files

    log_header("Executing STUBNETS pass (find stub nets).\n");

    // parse options
    size_t argidx;
    for (argidx = 1; argidx < args.size(); argidx++) {
      std::string arg = args[argidx];
      if (arg == "-o") {
        argidx++;
        outfile = fopen(args[argidx].c_str(), "w");
        continue;
      }
      break;
    }

    // handle extra options (e.g. selection)
    extra_args(args, argidx, design);

    ct.setup_internals();
    ct.setup_internals_mem();
    ct.setup_stdcells();     
    ct.setup_stdcells_mem();
    ct.setup_design(design);

    // call find_stub_nets() for each module that is either
    // selected as a whole or contains selected objects.
    for (auto module : design->selected_modules())
      find_stub_nets(design, module, outfile);
  }
} StubnetsPass;

PRIVATE_NAMESPACE_END
