

// TypeScript declarations for @stdlib/lapack/base/dsyevr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute selected eigenvalues and optionally eigenvectors of a real symmetric matrix
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
		IWORK: Int32Array,
		strideIWORK: number,
		offsetIWORK: number,
		liwork: number
	): Float64Array;
}

/**
* Compute selected eigenvalues and optionally eigenvectors of a real symmetric matrix
*/
declare var dsyevr: Routine;

export = dsyevr;
