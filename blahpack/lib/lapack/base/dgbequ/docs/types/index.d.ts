

// TypeScript declarations for @stdlib/lapack/base/dgbequ

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes row and column scalings to equilibrate a real general band matrix.
	*/
	(
		M: number,
		N: number,
		kl: number,
		ku: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
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
* Computes row and column scalings to equilibrate a real general band matrix.
*/
declare var dgbequ: Routine;

export = dgbequ;
