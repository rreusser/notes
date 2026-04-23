'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgerqf = require( './zgerqf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgerqf, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgerqf;
