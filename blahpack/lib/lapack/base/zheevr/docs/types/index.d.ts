

// TypeScript declarations for @stdlib/lapack/base/zheevr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes selected eigenvalues and eigenvectors of a complex Hermitian matrix using MRRR
	*/
	(
		jobz: string,
		range: string,
		uplo: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		vl: number,
		vu: number,
		il: number,
		iu: number,
		abstol: number,
		M: number,
		w: Float64Array,
		strideW: number,
		offsetW: number,
		Z: Float64Array,
		strideZ1: number,
		strideZ2: number,
		offsetZ: number,
		ISUPPZ: Int32Array,
		strideISUPPZ: number,
		offsetISUPPZ: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number,
		RWORK: Float64Array,
		strideRWORK: number,
		offsetRWORK: number,
		lrwork: number,
		IWORK: Int32Array,
		strideIWORK: number,
		offsetIWORK: number,
		liwork: number
	): Float64Array;
}

/**
* Computes selected eigenvalues and eigenvectors of a complex Hermitian matrix using MRRR
*/
declare var zheevr: Routine;

export = zheevr;
