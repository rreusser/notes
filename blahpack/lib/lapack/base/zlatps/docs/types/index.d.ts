

// TypeScript declarations for @stdlib/lapack/base/zlatps

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a complex triangular system with scaling to prevent overflow, where the matrix is in packed storage.
	*/
	(
		uplo: string,
		trans: string,
		diag: string,
		normin: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		scale: number,
		CNORM: Float64Array,
		strideCNORM: number,
		offsetCNORM: number
	): Float64Array;
}

/**
* Solves a complex triangular system with scaling to prevent overflow, where the matrix is in packed storage.
*/
declare var zlatps: Routine;

export = zlatps;
