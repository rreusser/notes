
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgghrd = require( './dgghrd.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgghrd, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgghrd;
