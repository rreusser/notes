

// TypeScript declarations for @stdlib/lapack/base/zgees

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute eigenvalues and Schur decomposition of a complex matrix
	*/
	(
		jobvs: string,
		sort: string,
		select: boolean,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		sdim: number,
		w: Float64Array,
		strideW: number,
		offsetW: number,
		VS: Float64Array,
		strideVS1: number,
		strideVS2: number,
		offsetVS: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number,
		RWORK: Float64Array,
		strideRWORK: number,
		offsetRWORK: number,
		BWORK: Float64Array,
		strideBWORK: number,
		offsetBWORK: number
	): Float64Array;
}

/**
* Compute eigenvalues and Schur decomposition of a complex matrix
*/
declare var zgees: Routine;

export = zgees;
