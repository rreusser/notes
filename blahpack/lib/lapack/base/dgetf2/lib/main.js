
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgetf2 = require( './dgetf2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgetf2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgetf2;
