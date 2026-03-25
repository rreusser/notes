

// TypeScript declarations for @stdlib/lapack/base/zgtcon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimate reciprocal condition number of complex tridiagonal matrix
	*/
	(
		norm: string,
		N: number,
		DL: Float64Array,
		strideDL: number,
		offsetDL: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		DU: Float64Array,
		strideDU: number,
		offsetDU: number,
		DU2: Float64Array,
		strideDU2: number,
		offsetDU2: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		anorm: number,
		rcond: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Estimate reciprocal condition number of complex tridiagonal matrix
*/
declare var zgtcon: Routine;

export = zgtcon;
