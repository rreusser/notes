

// TypeScript declarations for @stdlib/lapack/base/zgeev

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes eigenvalues and eigenvectors of a complex general matrix
	*/
	(
		jobvl: string,
		jobvr: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		w: Float64Array,
		strideW: number,
		offsetW: number,
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
		lwork: number,
		RWORK: Float64Array,
		strideRWORK: number,
		offsetRWORK: number
	): Float64Array;
}

/**
* Computes eigenvalues and eigenvectors of a complex general matrix
*/
declare var zgeev: Routine;

export = zgeev;
