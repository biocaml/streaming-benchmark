open Core
open Biocaml_unix
open Streaming_benchmark_unix

let () =
  Command.(
    let whole_thing =
      group ~summary:"Streaming benchmark" [
        ("bamcount", Bam_count_alignments.command);
        ("fagc", Fasta_gc.command);
        ("samcount", Sam_count_alignments.command);
        ("strymonas", Strymonas.command);
      ] in
    run whole_thing
  )
