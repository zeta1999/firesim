#!/usr/bin/env python

# See LICENSE for license details.

import fm_regex
import read_svf
import sys
import argparse

def initialize_arguments(args):
  """ initilize arguments """
  parser = argparse.ArgumentParser(
    description = 'Find match map between RTL Names & Gate-Level Names')
  parser.add_argument('--match', type=str, required=True, 
    help="""match output file (RTL name -> Gate-level name)""")
  parser.add_argument('--report', type=str, required=True,
    help="""report from Snopsys formality (generated by 'match' and 'report_match_points' )""")
  parser.add_argument('--svf', type=str, required=True,
    help="""decripted svf file from formality (generated in formality_svf/svf.txt)""")

  """ parse the arguments """
  res = parser.parse_args(args)

  return res.report, res.svf, res.match

def read_name_map(report_file, instance_map, change_names):
  name_map = list()

  with open(report_file, 'r') as f:
    ref_was_matched = False
    ref_name = ""
    for line in f:
      if ref_was_matched:
        impl_matched = fm_regex.impl_regex.search(line)
        if impl_matched:
          name_map.append((ref_name, impl_matched.group(1).replace("/", ".")))
          ref_was_matched = False

      else:
        ref_matched = fm_regex.ref_regex.search(line)
        if ref_matched:
          gate_type = ref_matched.group(1)
          ref_name_tokens = ref_matched.group(2).split("/")
          ref_name = ref_name_tokens[0]
          design = ref_name
          for i, token in enumerate(ref_name_tokens[1:]):
            if design in change_names:
              map = change_names[design]
              rtl_name = map[token] if token in map else token
            else:
              rtl_name = token
            ref_name = ref_name + "." + rtl_name
            if design in instance_map and i < len(ref_name_tokens[1:]) - 1:
              design = instance_map[design][rtl_name]
            else:
              design = ""

          if gate_type == "DFF":
            """ D Flip Flops """
            ff_matched = fm_regex.ff_regex.match(ref_name)
            reg_matched = fm_regex.reg_regex.match(ref_name)
            mem_matched = fm_regex.mem_regex.match(ref_name)
            if mem_matched:
              ref_name = mem_matched.group(1) + "[" + mem_matched.group(2) + "]" +\
                                                "[" + mem_matched.group(3) + "]"
            elif reg_matched:
              ref_name = reg_matched.group(1) + "[" + reg_matched.group(2) + "]"
            elif ff_matched:
              ref_name = ff_matched.group(1)
            else:
              print ref_name
              assert False 

          elif gate_type == "BBox":
            """ Macros """
            pass

          elif gate_type == "BlPin" or gate_type == "BBPin" or gate_type == "Port":
            """ Pins """
            bus_matched = fm_regex.bus_regex.search(ref_name)
            if bus_matched:
              ref_name = bus_matched.group(1) + "[" + bus_matched.group(2) + "]"

          else:
            assert False

          ref_was_matched = True

  return name_map

def write_match_file(match_file, name_map):
  with open(match_file, 'w') as f:
    for ref_name, impl_name in name_map:
      f.write("%s %s\n" % (ref_name, impl_name))

  return

if __name__ == '__main__':
  """ parse the arguments and open files """
  report_file, svf_file, match_file = initialize_arguments(sys.argv[1:])

  """ read svf file for guidance used in formality """
  instance_map, change_names = read_svf.read_svf_file(svf_file)

  """ read gate-level names from the formality report file """
  name_map = read_name_map(report_file, instance_map, change_names)

  """ write the output file """
  write_match_file(match_file, name_map)
