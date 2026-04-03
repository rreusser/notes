

// TypeScript declarations for @stdlib/lapack/base/zlags2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes 2-by-2 unitary matrices U, V, and Q for the generalized upper (lower) triangular form.
	*/
	(
		upper: boolean,
		a1: number,
		a2: any,
		a3: number,
		b1: number,
		b2: any,
		b3: number,
		csu: number,
		snu: any,
		csv: number,
		snv: any,
		csq: number,
		snq: any
	): void;
}

/**
* Computes 2-by-2 unitary matrices U, V, and Q for the generalized upper (lower) triangular form.
*/
declare var zlags2: Routine;

export = zlags2;
