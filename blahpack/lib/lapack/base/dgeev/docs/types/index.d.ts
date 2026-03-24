

// TypeScript declarations for @stdlib/lapack/base/dgeev

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes eigenvalues and eigenvectors of a real general matrix
	*/
	(
		jobvl: string,
		jobvr: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		WR: Float64Array,
		strideWR: number,
		offsetWR: number,
		WI: Float64Array,
		strideWI: number,
		offsetWI: number,
		VL: Float64Array,
		strideVL1: number,
		strideVL2: number,
		offsetVL: number,
		VR: Float64Array,
		strideVR1: number,
		strideVR2: number,
		offsetVR: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Computes eigenvalues and eigenvectors of a real general matrix
*/
declare var dgeev: Routine;

export = dgeev;
