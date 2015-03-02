open Core.Std
open Async.Std
open Bin_prot.Std

let hello_rpc = Rpc.Rpc.create
  ~name:"hello-world"
  ~version:0
  ~bin_query:String.bin_t
  ~bin_response:String.bin_t

let plus_one_rpc = Rpc.Rpc.create
  ~name:"plus-one"
  ~version:0
  ~bin_query:Int.bin_t
  ~bin_response:Int.bin_t

let sum_rpc = Rpc.Rpc.create
  ~name:"sum"
  ~version:0
  ~bin_query:(Array.bin_t Int.bin_t)
  ~bin_response:Int.bin_t

let add_rpc = Rpc.Rpc.create
  ~name:"add"
  ~version:0
  ~bin_query:Rpc_common.bin_two_int
  ~bin_response:Int.bin_t            

let record_rpc = Rpc.Rpc.create
  ~name:"record"
  ~version:0
  ~bin_query:Rpc_common.bin_record
  ~bin_response:Rpc_common.bin_record
