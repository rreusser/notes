
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlargv = require( './zlargv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlargv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlargv;
