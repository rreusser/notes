
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhptrd = require( './zhptrd.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhptrd, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhptrd;
