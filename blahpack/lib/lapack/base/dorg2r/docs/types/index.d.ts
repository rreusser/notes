

// TypeScript declarations for @stdlib/lapack/base/dorg2r

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Generate an orthogonal matrix from a QR factorization (unblocked)
	*/
	(
		M: number,
		N: number,
		K: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		TAU: Float64Array,
		strideTAU: number,
		offsetTAU: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Generate an orthogonal matrix from a QR factorization (unblocked)
*/
declare var dorg2r: Routine;

export = dorg2r;
