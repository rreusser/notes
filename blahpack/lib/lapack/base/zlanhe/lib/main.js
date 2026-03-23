

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zlanhe = require( './zlanhe.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlanhe, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlanhe;
