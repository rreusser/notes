

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dgetri = require( './dgetri.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgetri, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgetri;
