

// TypeScript declarations for @stdlib/lapack/base/ztgevc

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute eigenvectors of a pair of complex upper triangular matrices
	*/
	(
		side: string,
		howmny: string,
		SELECT: Float64Array,
		strideSELECT: number,
		offsetSELECT: number,
		N: number,
		S: Float64Array,
		strideS1: number,
		strideS2: number,
		offsetS: number,
		P: Float64Array,
		strideP1: number,
		strideP2: number,
		offsetP: number,
		VL: Float64Array,
		strideVL1: number,
		strideVL2: number,
		offsetVL: number,
		VR: Float64Array,
		strideVR1: number,
		strideVR2: number,
		offsetVR: number,
		mm: number,
		M: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		RWORK: Float64Array,
		strideRWORK: number,
		offsetRWORK: number
	): Float64Array;
}

/**
* Compute eigenvectors of a pair of complex upper triangular matrices
*/
declare var ztgevc: Routine;

export = ztgevc;
