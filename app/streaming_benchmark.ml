open Core.Std
open Biocaml_unix.Std
open Streaming_benchmark_unix

let () =
  Command.(
    let whole_thing =
      group ~summary:"Streaming benchmark" [
        ("bamcount", Bam_count_alignments.command);
        ("samcount", Sam_count_alignments.command);
      ] in
    run ~version:About.version whole_thing
  )
