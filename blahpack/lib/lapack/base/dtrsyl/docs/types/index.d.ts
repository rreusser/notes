

// TypeScript declarations for @stdlib/lapack/base/dtrsyl

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves the real Sylvester matrix equation
	*/
	(
		trana: string,
		tranb: string,
		isgn: number,
		M: number,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		C: Float64Array,
		strideC1: number,
		strideC2: number,
		offsetC: number,
		scale: number
	): Float64Array;
}

/**
* Solves the real Sylvester matrix equation
*/
declare var dtrsyl: Routine;

export = dtrsyl;
