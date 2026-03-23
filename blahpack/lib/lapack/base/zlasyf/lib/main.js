

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zlasyf = require( './zlasyf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlasyf, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlasyf;
