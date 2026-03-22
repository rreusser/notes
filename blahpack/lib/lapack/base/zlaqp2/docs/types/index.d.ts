

// TypeScript declarations for @stdlib/lapack/base/zlaqp2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* QR factorization with column pivoting (unblocked)
	*/
	(
		M: number,
		N: number,
		offset: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		JPVT: Int32Array,
		strideJPVT: number,
		offsetJPVT: number,
		TAU: Float64Array,
		strideTAU: number,
		offsetTAU: number,
		VN1: Float64Array,
		strideVN1: number,
		offsetVN1: number,
		VN2: Float64Array,
		strideVN2: number,
		offsetVN2: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* QR factorization with column pivoting (unblocked)
*/
declare var zlaqp2: Routine;

export = zlaqp2;
