

// TypeScript declarations for @stdlib/lapack/base/zgeequ

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute row and column scalings for a complex general matrix
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
* Compute row and column scalings for a complex general matrix
*/
declare var zgeequ: Routine;

export = zgeequ;
