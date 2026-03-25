

// TypeScript declarations for @stdlib/lapack/base/zgbcon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimate reciprocal condition number of complex general band matrix
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
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		anorm: number,
		rcond: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		RWORK: Float64Array,
		strideRWORK: number,
		offsetRWORK: number
	): Float64Array;
}

/**
* Estimate reciprocal condition number of complex general band matrix
*/
declare var zgbcon: Routine;

export = zgbcon;
