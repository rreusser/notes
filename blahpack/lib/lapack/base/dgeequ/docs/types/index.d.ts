

// TypeScript declarations for @stdlib/lapack/base/dgeequ

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes row and column scalings for equilibrating a general matrix
	*/
	(
		M: number,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		r: Float64Array,
		strideR: number,
		offsetR: number,
		c: Float64Array,
		strideC: number,
		offsetC: number,
		rowcnd: number,
		colcnd: number,
		amax: number
	): Float64Array;
}

/**
* Computes row and column scalings for equilibrating a general matrix
*/
declare var dgeequ: Routine;

export = dgeequ;
