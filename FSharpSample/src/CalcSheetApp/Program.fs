open System
open Library

//CALClite, a simple spreadsheet system, defined by the following type declarations:
type Row = int
type Col = char
type CellAddr = Row*Col
type ArithOp = Add | Sub | Mul | Div
type RangeOp = Sum | Count
type CellDef =
    FCst of float
    | SCst of string
    | Ref of CellAddr
    | RangeOp of CellAddr*CellAddr*RangeOp
    | ArithOp of CellDef*ArithOp*CellDef
type CellValue =
    S of string
    | F of float
type Sheet = Map<CellAddr,CellDef>;;

let header = [((1,'A'),SCst "#EYES");((1,'B'),SCst "1");((1,'C'),SCst "2");
              ((1,'D'),SCst "3");((1,'E'),SCst "4");((1,'F'),SCst "5");
              ((1,'G'),SCst "6");((1,'H'),SCst "Total")]
let result = [((2,'A'),SCst "RESULT");((2,'B'),FCst 2.0);((2,'C'),FCst 1.0);
              ((2,'D'),FCst 5.0);((2,'E'),FCst 0.0);((2,'F'),FCst 2.0);
              ((2,'G'),FCst 2.0);((2,'H'),RangeOp((2,'B'),(2,'G'),Sum))]
let calcPct col = ArithOp(FCst 100.0, Mul, ArithOp(Ref(2,col),Div,Ref(2,'H')))
let pct = [((3,'A'),SCst "PCT");((3,'B'),calcPct 'B');((3,'C'),calcPct 'C');
           ((3,'D'),calcPct 'D');((3,'E'),calcPct 'E');((3,'F'),calcPct 'F');
           ((3,'G'),calcPct 'G');((3,'H'),calcPct 'H')]
let dice = Map.ofList (header @ result @ pct);;

//EX 4.1
let header1 = [((4,'B'),SCst "NAME");((4,'C'), SCst "HEIGHT")]
let people = [((5,'B'),SCst "Hans");((5,'C'), FCst 167.40);
             ((6,'B'),SCst "Trine");((6,'C'), FCst 162.30);
             ((7,'B'),SCst "Peter");((7,'C'), FCst 179.70);
             ((8,'B'),SCst "");((8,'C'), SCst "")]
                                // how to make empty FCst?
let sumAvg = [((9,'B'), RangeOp((5,'B'),(7,'B'),Count));
              ((9,'C'), ArithOp(RangeOp((5,'C'),(7,'C'),Sum), Div, Ref(9,'B')))]
let height = Map.ofList(header1@people@sumAvg);;

//EX 4.2

let getF = function
    F f -> f
    | S s -> failwith "getF: expecting a float but got a string";;

let evalRangeOp xs op =
    match op with
    | Sum -> List.fold (fun acc x -> getF x + acc) 0.0 xs
             //List.fold (+) 0.0 (List.map getF xs)
    | Count -> float (List.length xs);;
               //List.fold (fun acc x -> acc + 1.0) 0.0 xs

let result0 = evalRangeOp [F 33.0; F 32.0] Sum;;
let result1 = evalRangeOp [] Sum;;
//let result2 = evalRangeOp [F 23.0; S "Hans"] Sum;; //NOT WORKING because : expecting a float but got a string
let result3 = evalRangeOp [F 23.0; S "Hans"] Count;;


let evalArithOp v1 v2 op =
    let f1 = getF v1
    let f2 = getF v2
    match op with
    | Add -> f1 + f2
    | Sub -> f1 - f2
    | Mul -> f1 * f2
    | Div -> if f2 = 0.0 
                then failwith "division by zero"
                else f1 / f2;;

let result4 = evalArithOp (F 33.0) (F 32.0) Sub;;
//evalArithOp (S "Hans") (F 1.0) Add;; //NOT WORKING because : expecting a float but got a string

//EX 4.3

let rec evalValue v sheet =
    match v with
    FCst f -> F f
    | SCst s -> S s
    | Ref ca -> evalCell ca sheet
    | RangeOp ((r1,c1),(r2,c2),op) -> 
        let xs = [for c=c1 to c2 do for r=r1 to r2 do yield evalCell(r,c) sheet ]
        F(evalRangeOp xs op)
    | ArithOp (v1,op,v2) -> 
        F (evalArithOp (evalValue v1 sheet) (evalValue v2 sheet) op)
and evalCell (ca:CellAddr) (sheet:Sheet) =
    match Map.tryFind ca sheet with
    None -> S "" // We define an empty cell to be the empty string value.
    | Some v -> evalValue v sheet;;

let result5 = evalCell (3,'G') dice;;

//EX 4.4

(* Find the column range and row range. 
You can do this by Map.toList on the sheet and look at the keys. 
You need to separate the keys in columns (letters) and rows (numbers). 
Then find the smallest and biggest columns (letters) and smallest and biggest rows (numbers).
In the dice example columns are from A to H and rows from 1 to 3. *)

let findRowRange (sheet:Sheet) =
    Map.fold (fun (aCmin,aCmax) (row,col) cV -> min row aCmin , max row aCmax ) (100,-100) sheet;;

let findColumnRange (sheet:Map<int*char,'b>) =
    Map.fold (fun (aCmin,aCmax) (row,col) cV -> min col aCmin , max col aCmax ) ('Z','A') sheet;;
                                                //compare minValue to maxZ, maxValue to minA
                                    //cV er CellDefinition -Fcst/Scst

let valueToString r c sheet =  
    match evalCell (r,char c) sheet with
    | S s -> s
    | F f -> sprintf"%.2f" f;; //sprintf print to string
                             //"%.2f" : float with to decimal

let ppBoard sheet = 
    let (minRow,maxRow) = findRowRange sheet
    let (minCol,maxCol) = findColumnRange sheet
    printf "    "
    for c= int minCol to int maxCol do printf"|%*c " 8 (char c)
        //casting to int to iterate                 casting to char to print
    printfn""
    printf "----"
    for c= int minCol to int maxCol do printf"+---------"
    printfn""
    for r= int minRow to int maxRow do
        printf"%*i " 3 r //printf"  %i " r
        for c= int minCol to int maxCol do printf"|%*s " 8 (valueToString r c sheet)
        printfn"";;

ppBoard dice;;
// > ppBoard dice;;
//     |       A |       B |       C |       D |       E |       F |       G |       H 
// ----+---------+---------+---------+---------+---------+---------+---------+---------
//   1 |   #EYES |       1 |       2 |       3 |       4 |       5 |       6 |   Total 
//   2 |  RESULT |    2.00 |    1.00 |    5.00 |    0.00 |    2.00 |    2.00 |   12.00 
//   3 |     PCT |   16.67 |    8.33 |   41.67 |    0.00 |   16.67 |   16.67 |  100.00 
// val it : unit = ()

ppBoard height;;
//> ppBoard height;;
//     |       B |       C 
// ----+---------+---------
//   4 |    NAME |  HEIGHT 
//   5 |    Hans |  167.40 
//   6 |   Trine |  162.30 
//   7 |   Peter |  179.70 
//   8 |         |         
//   9 |    3.00 |  169.80 
// val it : unit = ()
