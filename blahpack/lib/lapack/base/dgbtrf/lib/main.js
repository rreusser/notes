

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dgbtrf = require( './dgbtrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgbtrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgbtrf;
