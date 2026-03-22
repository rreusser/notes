

// TypeScript declarations for @stdlib/lapack/base/zgesvd

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the SVD of a complex matrix
	*/
	(
		jobu: string,
		jobvt: string,
		M: number,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		s: Float64Array,
		strideS: number,
		offsetS: number,
		U: Float64Array,
		strideU1: number,
		strideU2: number,
		offsetU: number,
		VT: Float64Array,
		strideVT1: number,
		strideVT2: number,
		offsetVT: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number,
		RWORK: Float64Array,
		strideRWORK: number,
		offsetRWORK: number
	): Float64Array;
}

/**
* Compute the SVD of a complex matrix
*/
declare var zgesvd: Routine;

export = zgesvd;
