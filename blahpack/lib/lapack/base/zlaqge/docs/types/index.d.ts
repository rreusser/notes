

// TypeScript declarations for @stdlib/lapack/base/zlaqge

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Equilibrate a complex general matrix using row and column scalings
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
		amax: number,
		equed: string
	): Float64Array;
}

/**
* Equilibrate a complex general matrix using row and column scalings
*/
declare var zlaqge: Routine;

export = zlaqge;
