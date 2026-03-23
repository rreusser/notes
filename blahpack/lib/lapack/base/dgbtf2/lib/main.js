

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dgbtf2 = require( './dgbtf2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgbtf2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgbtf2;
