module public NumberSystem

open System

// Type Definitions

type Number =
    | Integer of int64
    | Float of float
    | Rational of int64 * int64  // numerator, denominator
    | Complex of float * float    // real, imaginary

let private guardZero b op =
    match b with
    | Integer 0L | Float 0.0 -> failwith "Division by zero"
    | Rational (0L, _) -> failwith "Division by zero"
    | _ -> op ()

// Greatest Common Divisor using Euclidean algorithm
let rec private gcd a b =  // int64 arguments
    if b = 0L then abs a
    else gcd b (a % b)

// Simplify a rational number and avoid division by zero
let private simplifyRational (num: int64) (den: int64) =
    if den = 0L then
        failwith "Division by zero: denominator cannot be zero"
    let g = gcd num den
    let simplifiedNum = num / g
    let simplifiedDen = den / g
    // Keep denominator positive
    if simplifiedDen < 0L then
        (-simplifiedNum, -simplifiedDen)
    else
        (simplifiedNum, simplifiedDen)


// NUMBER CONVERSION //

let toFloat = function
    | Integer i -> float i
    | Float f -> f
    | Rational (n, d) -> float n / float d
    | Complex (r, _) -> r

let toComplex = function
    | Integer i -> (float i, 0.0)
    | Float f -> (f, 0.0)
    | Rational (n, d) -> (float n / float d, 0.0)
    | Complex (r, i) -> (r, i)

// Simplify a number to its simplest form
let simplifyNumber = function
    | Rational (n, d) when d = 1L -> Integer n
    | Rational (n, d) -> 
        let (sn, sd) = simplifyRational n d
        if sd = 1L then Integer sn
        else Rational (sn, sd)
    | Complex (r, i) when abs i < 1e-10 -> Float r
    | num -> num

// TYPE COERCION for static typing

let coerceToInt (num: Number) : Number =
    match num with
    | Integer i -> Integer i
    | Float f -> Integer (int64 f)
    | Rational (n, d) -> Integer (n / d)
    | Complex (r, _) -> Integer (int64 r)

let coerceToFloat (num: Number) : Number =
    Float (toFloat num)

let coerceToRational (num: Number) : Number =
    match num with
    | Integer i -> Rational (i, 1L)
    | Rational _ as r -> r
    | Float f -> 
        let denominator = 1000000L
        let numerator = int64 (f * float denominator)
        simplifyNumber (Rational (numerator, denominator))
    | Complex (r, _) -> 
        let denominator = 1000000L
        let numerator = int64 (r * float denominator)
        simplifyNumber (Rational (numerator, denominator))

let coerceToComplex (num: Number) : Number =
    let (r, i) = toComplex num
    Complex (r, i)


// ARITHMETIC OPERATIONS (Public Interface)

let add a b =
    match (a, b) with
    | Integer x, Integer y -> Integer (x + y)
    | Float x, Float y -> Float (x + y)
    | Float x, Integer y -> Float (x + float y)
    | Integer x, Float y -> Float (float x + y)
    | Rational (n1, d1), Rational (n2, d2) ->
        simplifyNumber (Rational (n1 * d2 + n2 * d1, d1 * d2))
    | Rational (n, d), Integer i ->
        simplifyNumber (Rational (n + i * d, d))
    | Integer i, Rational (n, d) ->
        simplifyNumber (Rational (i * d + n, d))
    | Rational (n, d), Float f ->
        Float (float n / float d + f)
    | Float f, Rational (n, d) ->
        Float (f + float n / float d)
    | Complex (r1, i1), Complex (r2, i2) ->
        simplifyNumber (Complex (r1 + r2, i1 + i2))
    | Complex (r, i), other | other, Complex (r, i) ->
        let (r2, i2) = toComplex other
        simplifyNumber (Complex (r + r2, i + i2))

let subtract a b =
    match (a, b) with
    | Integer x, Integer y -> Integer (x - y)
    | Float x, Float y -> Float (x - y)
    | Float x, Integer y -> Float (x - float y)
    | Integer x, Float y -> Float (float x - y)
    | Rational (n1, d1), Rational (n2, d2) ->
        simplifyNumber (Rational (n1 * d2 - n2 * d1, d1 * d2))
    | Rational (n, d), Integer i ->
        simplifyNumber (Rational (n - i * d, d))
    | Integer i, Rational (n, d) ->
        simplifyNumber (Rational (i * d - n, d))
    | Rational (n, d), Float f ->
        Float (float n / float d - f)
    | Float f, Rational (n, d) ->
        Float (f - float n / float d)
    | Complex (r1, i1), Complex (r2, i2) ->
        simplifyNumber (Complex (r1 - r2, i1 - i2))
    | Complex (r, i), other ->
        let (r2, i2) = toComplex other
        simplifyNumber (Complex (r - r2, i - i2))
    | other, Complex (r, i) ->
        let (r2, i2) = toComplex other
        simplifyNumber (Complex (r2 - r, i2 - i))

let multiply a b =
    match (a, b) with
    | Integer x, Integer y -> Integer (x * y)
    | Float x, Float y -> Float (x * y)
    | Float x, Integer y -> Float (x * float y)
    | Integer x, Float y -> Float (float x * y)
    | Rational (n1, d1), Rational (n2, d2) ->
        simplifyNumber (Rational (n1 * n2, d1 * d2))
    | Rational (n, d), Integer i ->
        simplifyNumber (Rational (n * i, d))
    | Integer i, Rational (n, d) ->
        simplifyNumber (Rational (i * n, d))
    | Rational (n, d), Float f ->
        Float (float n / float d * f)
    | Float f, Rational (n, d) ->
        Float (f * float n / float d)
    | Complex (r1, i1), Complex (r2, i2) ->
        simplifyNumber (Complex (r1 * r2 - i1 * i2, r1 * i2 + i1 * r2))
    | Complex (r, i), other | other, Complex (r, i) ->
        let (r2, i2) = toComplex other
        simplifyNumber (Complex (r * r2 - i * i2, r * i2 + i * r2))

let divide a b =
    match (a, b) with
    | Integer x, Integer y when y = 0L -> 
        failwith "Division by zero"
    | Integer x, Integer y when x % y = 0L -> 
        Integer (x / y)
    | Integer x, Integer y -> 
        simplifyNumber (Rational (x, y))
    | Float x, Float y when y = 0.0 -> 
        failwith "Division by zero"
    | Float x, Float y -> Float (x / y)
    | Float x, Integer y when y = 0L ->
        failwith "Division by zero"
    | Float x, Integer y -> Float (x / float y)
    | Integer x, Float y when y = 0.0 ->
        failwith "Division by zero"
    | Integer x, Float y -> Float (float x / y)
    | Rational (n1, d1), Rational (n2, d2) when n2 = 0L ->
        failwith "Division by zero"
    | Rational (n1, d1), Rational (n2, d2) ->
        simplifyNumber (Rational (n1 * d2, d1 * n2))
    | Rational (n, d), Integer i when i = 0L ->
        failwith "Division by zero"
    | Rational (n, d), Integer i ->
        simplifyNumber (Rational (n, d * i))
    | Integer i, Rational (n, d) when n = 0L ->
        failwith "Division by zero"
    | Integer i, Rational (n, d) ->
        simplifyNumber (Rational (i * d, n))
    | Rational (n, d), Float f when f = 0.0 ->
        failwith "Division by zero"
    | Rational (n, d), Float f ->
        Float (float n / float d / f)
    | Float f, Rational (n, d) when n = 0L ->
        failwith "Division by zero"
    | Float f, Rational (n, d) ->
        Float (f * float d / float n)
    | Complex (r1, i1), Complex (r2, i2) when r2 = 0.0 && i2 = 0.0 ->
        failwith "Division by zero"
    | Complex (r1, i1), Complex (r2, i2) ->
        let denominator = r2 * r2 + i2 * i2
        simplifyNumber (Complex ((r1 * r2 + i1 * i2) / denominator, 
                                 (i1 * r2 - r1 * i2) / denominator))
    | Complex (r, i), other ->
        let (r2, i2) = toComplex other
        if r2 = 0.0 && i2 = 0.0 then
            failwith "Division by zero"
        let denominator = r2 * r2 + i2 * i2
        simplifyNumber (Complex ((r * r2 + i * i2) / denominator, 
                                 (i * r2 - r * i2) / denominator))
    | other, Complex (r, i) ->
        let (r2, i2) = toComplex other
        if r = 0.0 && i = 0.0 then
            failwith "Division by zero"
        let denominator = r * r + i * i
        simplifyNumber (Complex ((r2 * r + i2 * i) / denominator, 
                                 (i2 * r - r2 * i) / denominator))

let modulo a b =
    match (a, b) with
    | Integer x, Integer y when y = 0L ->
        failwith "Modulo by zero"
    | Integer x, Integer y -> Integer (x % y)
    | _ -> 
        let fa = toFloat a
        let fb = toFloat b
        if fb = 0.0 then failwith "Modulo by zero"
        Float (fa % fb)

let power a b =
    match (a, b) with
    | Integer x, Integer y when y >= 0L ->
        Integer (pown x (int y))
    | Complex (r, i), Integer y ->
        // Integer power of complex number
        let rec complexPower (r: float, i: float) (exp: int) =
            if exp = 0 then (1.0, 0.0)
            elif exp = 1 then (r, i)
            elif exp < 0 then
                // Negative power: (a+bi)^(-n) = 1 / (a+bi)^n
                let (rPos, iPos) = complexPower (r, i) (-exp)
                let denom = rPos * rPos + iPos * iPos
                (rPos / denom, -iPos / denom)
            else
                let (r2, i2) = complexPower (r, i) (exp - 1)
                (r * r2 - i * i2, r * i2 + i * r2)
        let (realResult, imagResult) = complexPower (r, i) (int y)
        simplifyNumber (Complex (realResult, imagResult))
    | Complex (r, i), _ ->
        // General complex power using: (a+bi)^c = e^(c*ln(a+bi))
        // ln(a+bi) = ln(|a+bi|) + i*arg(a+bi)
        let magnitude = sqrt(r * r + i * i)
        let angle = atan2 i r
        let expVal = toFloat b
        let newMagnitude = magnitude ** expVal
        let newAngle = angle * expVal
        simplifyNumber (Complex (newMagnitude * cos(newAngle), newMagnitude * sin(newAngle)))
    | _ ->
        let fa = toFloat a
        let fb = toFloat b
        Float (fa ** fb)

let negate = function
    | Integer i -> Integer (-i)
    | Float f -> Float (-f)
    | Rational (n, d) -> Rational (-n, d)
    | Complex (r, i) -> Complex (-r, -i)


// STRING REPRESENTATION

let toString = function
    | Integer i -> string i
    | Float f -> string f
    | Rational (n, d) when d = 1L -> string n
    | Rational (n, d) -> sprintf "%d/%d" n d
    | Complex (r, 0.0) -> string r
    | Complex (0.0, i) -> sprintf "%gi" i
    | Complex (r, i) when i >= 0.0 -> sprintf "%g+%gi" r i
    | Complex (r, i) -> sprintf "%g%gi" r i