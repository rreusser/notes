

// TypeScript declarations for @stdlib/lapack/base/zggev

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the generalized eigenvalues and optionally the eigenvectors of a complex matrix pair (A, B).
	*/
	(
		jobvl: string,
		jobvr: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		ALPHA: Float64Array,
		strideALPHA: number,
		offsetALPHA: number,
		BETA: Float64Array,
		strideBETA: number,
		offsetBETA: number,
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
* Compute the generalized eigenvalues and optionally the eigenvectors of a complex matrix pair (A, B).
*/
declare var zggev: Routine;

export = zggev;
