let screen_width = 400
let screen_height = 400

let cols = 20
let rows = 20

let cell_width = screen_width/cols
let cell_height = screen_height/rows

let main_grid = Array.make_matrix rows cols 0

(* blinker *)
(*
let () = main_grid.(10).(10) <- 1
let () = main_grid.(10).(11) <- 1
let () = main_grid.(10).(12) <- 1
*)


(* blinker *)
let () = main_grid.(8).(8) <- 1
let () = main_grid.(8).(9) <- 1
let () = main_grid.(9).(8) <- 1
let () = main_grid.(11).(11) <- 1
let () = main_grid.(11).(10) <- 1
let () = main_grid.(10).(11) <- 1

let get_cell_state grid (j,k) = try grid.(j).(k) with Invalid_argument _ -> 0


let calculate_live_neighbors grid (j,k) = 
        get_cell_state grid (j+1,k) +
        get_cell_state grid (j-1,k) +
        get_cell_state grid (j,k-1) +
        get_cell_state grid (j,k+1) +
        get_cell_state grid (j-1,k-1) +
        get_cell_state grid (j+1,k+1) +
        get_cell_state grid (j-1,k+1) +
        get_cell_state grid (j+1,k-1) 


let new_state grid (j, k) = 
        match (get_cell_state grid (j,k) , calculate_live_neighbors grid (j,k)) with
        | state, x when state = 1 && x < 2 -> 0
        | state,x when state = 1 && (x = 2 || x = 3) -> 1
        | state, x when state = 1 && x > 3 -> 0
        | state,x when state = 0 && x = 3 -> 1
        | _, _ -> 0
        (*| state, x -> failwith ("unknown condition with state: " ^ Int.to_string state ^ " live neighbors: " ^ Int.to_string x)*)

let tick grid = 
        let new_grid = Array.make_matrix rows cols 0 in
        Array.iteri (fun j inner_array ->
           Array.iteri (fun k _ ->
                new_grid.(j).(k) <- new_state grid (j, k);
            ) inner_array
        ) new_grid; new_grid

let get_live_cells_pos grid = 
        List.mapi (fun x inner_array -> 
            let inner_list = Array.to_list inner_array in
                List.filter_map (Fun.id) @@ List.mapi (fun y state -> 
                if state = 1 then Some (x, y) else None 
                ) inner_list ) (Array.to_list grid) |> List.flatten
        
        
let make_cell_live (x,y) = Raylib.draw_rectangle (x*cell_width) (y*cell_height) cell_width cell_height Raylib.Color.black

let setup () =
  Raylib.init_window screen_width screen_height "Game Of Life";
  Raylib.set_target_fps 3


let rec loop grid =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();
    clear_background Color.raywhite;

    let c = List.init cols Fun.id in
    let r =  List.init rows Fun.id in

    List.iter (fun i -> 
      List.iter (fun j -> 
        draw_rectangle_lines (j*cell_width) (i*cell_height) cell_width cell_height Color.black;
      ) c;
    ) r;

    List.iter (fun (x, y) -> make_cell_live (x,y)) (get_live_cells_pos grid);

    end_drawing ();
    loop (tick grid)

let () = setup (); loop main_grid


let () = print_endline (Int.to_string @@ calculate_live_neighbors main_grid (50,50))



