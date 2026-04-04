

// TypeScript declarations for @stdlib/lapack/base/dlangb

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Returns the norm of a real general band matrix.
	*/
	(
		norm: string,
		N: number,
		kl: number,
		ku: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Returns the norm of a real general band matrix.
*/
declare var dlangb: Routine;

export = dlangb;
