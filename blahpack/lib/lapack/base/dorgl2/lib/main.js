

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dorgl2 = require( './dorgl2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dorgl2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dorgl2;
