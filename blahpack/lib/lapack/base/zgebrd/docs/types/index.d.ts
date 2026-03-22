

// TypeScript declarations for @stdlib/lapack/base/zgebrd

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reduce a complex matrix to bidiagonal form (blocked)
	*/
	(
		M: number,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		e: Float64Array,
		strideE: number,
		offsetE: number,
		TAUQ: Float64Array,
		strideTAUQ: number,
		offsetTAUQ: number,
		TAUP: Float64Array,
		strideTAUP: number,
		offsetTAUP: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Reduce a complex matrix to bidiagonal form (blocked)
*/
declare var zgebrd: Routine;

export = zgebrd;
