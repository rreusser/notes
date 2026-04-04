
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhbev = require( './zhbev.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhbev, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhbev;
